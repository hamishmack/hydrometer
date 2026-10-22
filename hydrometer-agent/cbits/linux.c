/* Filesystem space for the linux agent (the unix package has no statvfs). */
#include <stdint.h>
#include <sys/statvfs.h>

int hm_fs_space(const char *path, int64_t *out)
{
    struct statvfs sf;
    if (statvfs(path, &sf) != 0)
        return -1;
    out[0] = (int64_t)sf.f_bavail * (int64_t)sf.f_frsize;
    out[1] = (int64_t)sf.f_blocks * (int64_t)sf.f_frsize;
    return 0;
}
