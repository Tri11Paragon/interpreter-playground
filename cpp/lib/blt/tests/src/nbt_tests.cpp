/*
 * Created by Brett on 11/08/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */
#include "nbt_tests.h"
#include <blt/fs/nbt.h>
#include <blt/profiling/profiler.h>
#include <blt/std/logging.h>
#include <blt/format/format.h>
#include <blt/fs/filesystem.h>
#include <filesystem>

void blt::tests::nbtFSBlockRead(size_t buff_size, size_t arr_size, size_t num_array, unsigned char** arr)
{

}

void blt::tests::nbtFSBlockWrite(size_t buff_size, size_t arr_size, size_t num_array, unsigned char** arr)
{
    std::fstream out("fs_tests/test_block_fs_" + std::to_string(buff_size) + ".all", std::ios::out | std::ios::binary);
    blt::fs::fstream_block_writer writer(out, buff_size);
    
    auto str = "fs::block::" + std::to_string(buff_size);
    BLT_START_INTERVAL("Write Tests", str);
    for (size_t _ = 0; _ < num_array; _++)
        writer.write(reinterpret_cast<char*>(arr[_]), arr_size);
    BLT_END_INTERVAL("Write Tests", str);
}

void blt::tests::nbtFSRead(size_t buff_size, size_t arr_size, size_t num_array, unsigned char** arr)
{

}

void blt::tests::nbtFSWrite(size_t buff_size, size_t arr_size, size_t num_array, unsigned char** arr)
{
    std::fstream out("fs_tests/test_fs_" + std::to_string(buff_size) + ".all", std::ios::out | std::ios::binary);
    
    auto str = "fs::std::" + std::to_string(buff_size);
    BLT_START_INTERVAL("Write Tests", str);
    for (size_t _ = 0; _ < num_array; _++)
        out.write(reinterpret_cast<const char*>(arr[_]), (long)arr_size);
    BLT_END_INTERVAL("Write Tests", str);
}

void blt::tests::nbtFSTest(size_t total_size)
{
    std::filesystem::create_directory("fs_tests/");
    for (int i = 10; i < 20; i++)
    {
        auto buff_size = (size_t) std::pow(2, i);
        auto arr_size = 512 * 1;
        auto num_array = (total_size) / arr_size;
        
        auto* arr = new unsigned char* [num_array];
        for (size_t _ = 0; _ < num_array; _++)
        {
            arr[_] = new unsigned char[arr_size];
            generateRandomData(arr[_], arr_size, _);
        }
        
        nbtFSBlockWrite(buff_size, arr_size, num_array, arr);
        nbtFSWrite(buff_size, arr_size, num_array, arr);
        
        for (size_t _ = 0; _ < num_array; _++)
            delete[] arr[_];
        delete[] arr;
    }
    BLT_PRINT_PROFILE("Write Tests");
}

void blt::tests::nbtRawRead()
{

}

void blt::tests::nbtRawWrite()
{

}

void blt::tests::nbtRawTest()
{

}

void blt::tests::nbtRead()
{
    std::fstream nbtInputFile("super_file.nbt", std::ios::in | std::ios::binary);
    blt::fs::fstream_block_reader blockReader(nbtInputFile);
    blt::nbt::NBTReader nbtReader(blockReader);
    nbtReader.read();
    
    auto shortTag = nbtReader.getTag<blt::nbt::tag_short>("shortTest");
    BLT_TRACE("Got short: %d", shortTag->get());
}

void blt::tests::nbtWrite()
{
    std::fstream nbtFile("super_file.nbt", std::ios::out | std::ios::binary);
    blt::fs::fstream_block_writer blockWriter(nbtFile);
    blt::nbt::NBTWriter nbtWriter(blockWriter);
    nbtWriter.write(
            new blt::nbt::tag_compound(
                    "root", {
                            new blt::nbt::tag_byte("super_byte", 8),
                            new blt::nbt::tag_short("shortTest", 32767),
                            new blt::nbt::tag_compound(
                                    "SEXY_COMPOUND", {
                                            new blt::nbt::tag_list(
                                                    "my list", {
                                                            new blt::nbt::tag_long("", 1230),
                                                            new blt::nbt::tag_long("", 2),
                                                            new blt::nbt::tag_long("", 50340535),
                                                            new blt::nbt::tag_long("", 55),
                                                            new blt::nbt::tag_long("", 256),
                                                            new blt::nbt::tag_long("", 512),
                                                            new blt::nbt::tag_long("", 9999999999),
                                                    }
                                            ),
                                            new blt::nbt::tag_double("OMG IT'S A DOUBLE", 1320.04324),
                                            new blt::nbt::tag_float("OMG IT'S A FLOAT", 12.04324),
                                            new blt::nbt::tag_compound(
                                                    "Triple", {
                                                            new blt::nbt::tag_int("Test int", 32),
                                                            new blt::nbt::tag_byte_array(
                                                                    "super array", {
                                                                            51, 23, 12, 04, 33, 53, 11, 22, 3, 93, 120
                                                                    }
                                                            ),
                                                            new blt::nbt::tag_string("I am a string", "I have stringy contents"),
                                                            new blt::nbt::tag_string("name", "Bananrama"),
                                                            new blt::nbt::tag_int_array(
                                                                    "int array", {
                                                                            1230, 234023, 21300, 2309230, 2340230, 2, 1, 32, 3265, 12, 53, 123, 7,
                                                                            56, 12
                                                                    }
                                                            ),
                                                            new blt::nbt::tag_long_array(
                                                                    "valid", {
                                                                            1230, 5320, 323200234402304, 230023, 23042034, 230230, 2301203,
                                                                            123010230, 12300123
                                                                    }
                                                            )
                                                    }
                                            )
                                    }
                            )
                    }
            ));
    
    blockWriter.flush();
    nbtFile.close();
}

void blt::tests::nbtTest()
{

}
