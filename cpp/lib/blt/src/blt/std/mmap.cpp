/*
 *  <Short Description>
 *  Copyright (C) 2024  Brett Terpstra
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include <blt/logging/logging.h>
#include <blt/std/mmap.h>

#ifdef __unix__
    
    #include <sys/mman.h>

#else
#include <blt/std/assert.h>
#endif

namespace blt
{
    
    std::string handle_mmap_error()
    {
        std::string str;
#define BLT_WRITE(arg) str += arg; str += '\n';
        switch (errno)
        {
            case EACCES:
            BLT_WRITE("fd not set to open!");
                break;
            case EAGAIN:
            BLT_WRITE("The file has been locked, or too much memory has been locked");
                break;
            case EBADF:
            BLT_WRITE("fd is not a valid file descriptor");
                break;
            case EEXIST:
            BLT_WRITE("MAP_FIXED_NOREPLACE was specified in flags, and the range covered "
                      "by addr and length clashes with an existing mapping.");
                break;
            case EINVAL:
            BLT_WRITE("We don't like addr, length, or offset (e.g., they are too large, or not aligned on a page boundary).");
                BLT_WRITE("Or length was 0");
                BLT_WRITE("Or flags contained none of MAP_PRIVATE, MAP_SHARED, or MAP_SHARED_VALIDATE.");
                break;
            case ENFILE:
            BLT_WRITE("The system-wide limit on the total number of open files has been reached.");
                break;
            case ENODEV:
            BLT_WRITE("The underlying filesystem of the specified file does not support memory mapping.");
                break;
            case ENOMEM:
            BLT_WRITE("No memory is available.");
                BLT_WRITE("Or The process's maximum number of mappings would have been exceeded.  "
                          "This error can also occur for munmap(), when unmapping a region in the middle of an existing mapping, "
                          "since this results in two smaller mappings on either side of the region being unmapped.");
                BLT_WRITE("Or The process's RLIMIT_DATA limit, described in getrlimit(2), would have been exceeded.");
                BLT_WRITE("Or We don't like addr, because it exceeds the virtual address space of the CPU.");
                break;
            case EOVERFLOW:
            BLT_WRITE("On 32-bit architecture together with the large file extension (i.e., using 64-bit off_t): "
                      "the number of pages used for length plus number of "
                      "pages used for offset would overflow unsigned long (32 bits).");
                break;
            case EPERM:
            BLT_WRITE("The prot argument asks for PROT_EXEC but the mapped area "
                      "belongs to a file on a filesystem that was mounted no-exec.");
                BLT_WRITE("Or The operation_t was prevented by a file seal");
                BLT_WRITE("Or The MAP_HUGETLB flag was specified, but the caller "
                          "was not privileged (did not have the CAP_IPC_LOCK capability) "
                          "and is not a member of the sysctl_hugetlb_shm_group group; "
                          "see the description of /proc/sys/vm/sysctl_hugetlb_shm_group");
                break;
            case ETXTBSY:
            BLT_WRITE("MAP_DENYWRITE was set but the object specified by fd is open for writing.");
                break;
        }
        return str;
    }
    
    void* allocate_huge_pages(huge_page_t page_type, blt::size_t bytes)
    {
#ifdef __unix__
        auto type = (21 << MAP_HUGE_SHIFT);
        if (page_type == huge_page_t::BLT_1GB_PAGE)
            type = (30 << MAP_HUGE_SHIFT);
        
        auto buffer = mmap(nullptr, bytes, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_HUGETLB | type | MAP_POPULATE, -1, 0);
        if (buffer == MAP_FAILED)
        {
            throw bad_alloc_t(handle_mmap_error());
        }
        return buffer;
#else
        (void)page_type;
        (void)bytes;
        BLT_ABORT("Platform not supported for huge page allocation!");
#endif
    }
    
    void mmap_free(void* ptr, blt::size_t bytes)
    {
#ifdef __unix__
        if (munmap(ptr, bytes))
        {
            BLT_ERROR("Failed to deallocate");
            throw bad_alloc_t(handle_mmap_error());
        }
#else
        (void)ptr;
        (void)bytes;
#endif
    }
}