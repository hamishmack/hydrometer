/* Host and per-process statistics for macOS.
 *
 * Everything here is a thin, allocation-free wrapper over a Darwin API; the
 * Haskell side (Hydrometer.Agent.Darwin) owns buffers and does the
 * arithmetic.  Times are returned in nanoseconds: rusage times are in Mach
 * absolute-time units, which are not nanoseconds on Apple Silicon. */

#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <libproc.h>
#include <mach/mach.h>
#include <mach/mach_time.h>
#include <net/if.h>
#include <net/route.h>
#include <sys/mount.h>
#include <sys/proc_info.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/sysctl.h>
#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/storage/IOBlockStorageDriver.h>

static double mach_to_ns(void)
{
    static double factor = 0;
    if (factor == 0) {
        mach_timebase_info_data_t tb;
        mach_timebase_info(&tb);
        factor = (double)tb.numer / (double)tb.denom;
    }
    return factor;
}

/* out: user, system, idle, nice -- milliseconds of CPU time over all cores
 * since boot. */
int hm_host_cpu(uint64_t *out)
{
    host_cpu_load_info_data_t info;
    mach_msg_type_number_t count = HOST_CPU_LOAD_INFO_COUNT;
    if (host_statistics(mach_host_self(), HOST_CPU_LOAD_INFO,
                        (host_info_t)&info, &count) != KERN_SUCCESS)
        return -1;
    long tck = sysconf(_SC_CLK_TCK);
    if (tck <= 0) tck = 100;
    for (int i = 0; i < 4; i++) {
        static const int idx[4] = { CPU_STATE_USER, CPU_STATE_SYSTEM,
                                    CPU_STATE_IDLE, CPU_STATE_NICE };
        out[i] = (uint64_t)info.cpu_ticks[idx[i]] * 1000 / (uint64_t)tck;
    }
    return 0;
}

/* out: used, compressed, free, pageouts(cumulative pages), swapouts
 * (cumulative pages), page size, swap used, swap total, memorystatus level
 * (percent available, or -1). */
int hm_host_mem(int64_t *out)
{
    vm_statistics64_data_t vm;
    mach_msg_type_number_t count = HOST_VM_INFO64_COUNT;
    if (host_statistics64(mach_host_self(), HOST_VM_INFO64,
                          (host_info64_t)&vm, &count) != KERN_SUCCESS)
        return -1;
    int64_t page = (int64_t)vm_kernel_page_size;
    /* Activity Monitor's "Memory Used": app memory + wired + compressed. */
    int64_t app = (int64_t)vm.internal_page_count - (int64_t)vm.purgeable_count;
    out[0] = (app + (int64_t)vm.wire_count + (int64_t)vm.compressor_page_count) * page;
    out[1] = (int64_t)vm.compressor_page_count * page;
    out[2] = ((int64_t)vm.free_count - (int64_t)vm.speculative_count) * page;
    out[3] = (int64_t)vm.pageouts;
    out[4] = (int64_t)vm.swapouts;
    out[5] = page;

    struct xsw_usage sw;
    size_t len = sizeof(sw);
    if (sysctlbyname("vm.swapusage", &sw, &len, NULL, 0) == 0) {
        out[6] = (int64_t)sw.xsu_used;
        out[7] = (int64_t)sw.xsu_total;
    } else {
        out[6] = out[7] = 0;
    }
    int level = -1;
    len = sizeof(level);
    if (sysctlbyname("kern.memorystatus_level", &level, &len, NULL, 0) != 0)
        level = -1;
    out[8] = level;
    return 0;
}

/* out: kern.num_files, kern.maxfiles, boot time (ms since epoch). */
int hm_host_misc(int64_t *out)
{
    int v = 0;
    size_t len = sizeof(v);
    out[0] = sysctlbyname("kern.num_files", &v, &len, NULL, 0) == 0 ? v : -1;
    len = sizeof(v);
    out[1] = sysctlbyname("kern.maxfiles", &v, &len, NULL, 0) == 0 ? v : -1;
    struct timeval bt;
    len = sizeof(bt);
    int mib[2] = { CTL_KERN, KERN_BOOTTIME };
    out[2] = sysctl(mib, 2, &bt, &len, NULL, 0) == 0
           ? (int64_t)bt.tv_sec * 1000 + bt.tv_usec / 1000 : 0;
    return 0;
}

/* Per-interface 64-bit byte counters.  Fills up to max entries of
 * names (16 bytes each), rx and tx; returns the number filled. */
int hm_net_ifaces(char *names, uint64_t *rx, uint64_t *tx, int max)
{
    int mib[6] = { CTL_NET, PF_ROUTE, 0, 0, NET_RT_IFLIST2, 0 };
    size_t len = 0;
    if (sysctl(mib, 6, NULL, &len, NULL, 0) < 0)
        return -1;
    char *buf = malloc(len);
    if (!buf)
        return -1;
    if (sysctl(mib, 6, buf, &len, NULL, 0) < 0) {
        free(buf);
        return -1;
    }
    int n = 0;
    for (char *p = buf; p < buf + len && n < max; ) {
        struct if_msghdr *ifm = (struct if_msghdr *)p;
        if (ifm->ifm_msglen == 0)
            break;
        if (ifm->ifm_type == RTM_IFINFO2) {
            struct if_msghdr2 *if2 = (struct if_msghdr2 *)ifm;
            char name[IF_NAMESIZE];
            if (if_indextoname(if2->ifm_index, name) && (if2->ifm_flags & IFF_UP)) {
                strncpy(names + 16 * n, name, 15);
                names[16 * n + 15] = 0;
                rx[n] = if2->ifm_data.ifi_ibytes;
                tx[n] = if2->ifm_data.ifi_obytes;
                n++;
            }
        }
        p += ifm->ifm_msglen;
    }
    free(buf);
    return n;
}

/* Cumulative bytes read and written by all block storage drivers. */
int hm_disk_io(uint64_t *out)
{
    out[0] = out[1] = 0;
    io_iterator_t it;
    if (IOServiceGetMatchingServices(kIOMainPortDefault,
                                     IOServiceMatching(kIOBlockStorageDriverClass),
                                     &it) != KERN_SUCCESS)
        return -1;
    io_registry_entry_t drive;
    while ((drive = IOIteratorNext(it))) {
        CFDictionaryRef stats = (CFDictionaryRef)IORegistryEntryCreateCFProperty(
            drive, CFSTR(kIOBlockStorageDriverStatisticsKey), kCFAllocatorDefault, 0);
        if (stats) {
            CFNumberRef num;
            int64_t v;
            num = CFDictionaryGetValue(stats, CFSTR(kIOBlockStorageDriverStatisticsBytesReadKey));
            if (num && CFNumberGetValue(num, kCFNumberSInt64Type, &v)) out[0] += (uint64_t)v;
            num = CFDictionaryGetValue(stats, CFSTR(kIOBlockStorageDriverStatisticsBytesWrittenKey));
            if (num && CFNumberGetValue(num, kCFNumberSInt64Type, &v)) out[1] += (uint64_t)v;
            CFRelease(stats);
        }
        IOObjectRelease(drive);
    }
    IOObjectRelease(it);
    return 0;
}

/* out: free bytes, total bytes of the filesystem holding path. */
int hm_fs_space(const char *path, int64_t *out)
{
    struct statfs sf;
    if (statfs(path, &sf) != 0)
        return -1;
    out[0] = (int64_t)sf.f_bavail * (int64_t)sf.f_bsize;
    out[1] = (int64_t)sf.f_blocks * (int64_t)sf.f_bsize;
    return 0;
}

/* One process.  Keep in sync with the Storable instance in
 * Hydrometer.Agent.Darwin. */
struct hm_proc {
    int32_t pid;
    int32_t ppid;
    uint32_t uid;
    int32_t pad;
    int64_t start_ms;
    int64_t cpu_ns;         /* own user + system */
    int64_t child_cpu_ns;   /* reaped children, user + system */
    int64_t rss;
    int64_t footprint;
    int64_t disk_read;
    int64_t disk_write;
    char comm[32];
    char path[256];
};

int hm_proc_size(void) { return sizeof(struct hm_proc); }

/* Fill up to max processes; returns the number filled.  Processes that exit
 * mid-scan are skipped. */
int hm_procs(struct hm_proc *out, int max)
{
    int cap = proc_listallpids(NULL, 0);
    if (cap <= 0)
        return -1;
    cap += 64;
    pid_t *pids = malloc(sizeof(pid_t) * cap);
    if (!pids)
        return -1;
    int npids = proc_listallpids(pids, sizeof(pid_t) * cap);
    double f = mach_to_ns();
    int n = 0;
    for (int i = 0; i < npids && n < max; i++) {
        pid_t pid = pids[i];
        if (pid == 0)
            continue;
        struct proc_bsdinfo bi;
        if (proc_pidinfo(pid, PROC_PIDTBSDINFO, 0, &bi, sizeof(bi)) != sizeof(bi))
            continue;
        struct rusage_info_v4 ru;
        if (proc_pid_rusage(pid, RUSAGE_INFO_V4, (rusage_info_t *)&ru) != 0)
            continue;
        struct hm_proc *p = &out[n];
        memset(p, 0, sizeof(*p));
        p->pid = pid;
        p->ppid = (int32_t)bi.pbi_ppid;
        p->uid = bi.pbi_uid;
        p->start_ms = (int64_t)bi.pbi_start_tvsec * 1000 + (int64_t)bi.pbi_start_tvusec / 1000;
        p->cpu_ns = (int64_t)((double)(ru.ri_user_time + ru.ri_system_time) * f);
        p->child_cpu_ns = (int64_t)((double)(ru.ri_child_user_time + ru.ri_child_system_time) * f);
        p->rss = (int64_t)ru.ri_resident_size;
        p->footprint = (int64_t)ru.ri_phys_footprint;
        p->disk_read = (int64_t)ru.ri_diskio_bytesread;
        p->disk_write = (int64_t)ru.ri_diskio_byteswritten;
        strncpy(p->comm, bi.pbi_name[0] ? bi.pbi_name : bi.pbi_comm, sizeof(p->comm) - 1);
        char path[PROC_PIDPATHINFO_MAXSIZE];
        if (proc_pidpath(pid, path, sizeof(path)) > 0)
            strncpy(p->path, path, sizeof(p->path) - 1);
        n++;
    }
    free(pids);
    return n;
}

/* The raw KERN_PROCARGS2 blob (argc, exec path, argv, environment) for pid.
 * Returns the number of bytes written, or -1. */
int hm_procargs(int pid, char *buf, int buflen)
{
    int mib[3] = { CTL_KERN, KERN_PROCARGS2, pid };
    size_t len = (size_t)buflen;
    if (sysctl(mib, 3, buf, &len, NULL, 0) != 0)
        return -1;
    return (int)len;
}
