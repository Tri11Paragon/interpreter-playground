//
// Created by brett on 15/08/23.
//

#if !defined(BLT_UUID_H) && defined(__has_include) && __has_include(<openssl/sha.h>)
#define BLT_UUID_H

#include <string>
#include <openssl/sha.h>
#include <random>
#include <exception>
#include <blt/std/string.h>
#include <iomanip>
#include <sstream>
#include <cstring>

namespace blt::uuid
{
    
    // from https://www.rfc-editor.org/rfc/rfc4122#section-4.3
    
    union uuid_t
    {
        struct
        {
            u_int32_t time_low;
            u_int16_t time_mid;
            u_int16_t time_hi_and_version;
            u_int8_t clock_seq_hi_and_reserved;
            u_int8_t clock_seq_low;
            u_int8_t node[6];
        } uuid;
        u_int8_t str[16];
    };
    
    /* Name string is a fully-qualified domain name */
    constexpr static uuid_t NameSpace_DNS = { /* 6ba7b810-9dad-11d1-80b4-00c04fd430c8 */
            {0x6ba7b810,
             0x9dad,
             0x11d1,
             0x80, 0xb4, {0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8}}
    };
    
    /* Name string is a URL */
    constexpr static uuid_t NameSpace_URL = { /* 6ba7b811-9dad-11d1-80b4-00c04fd430c8 */
            {0x6ba7b811,
             0x9dad,
             0x11d1,
             0x80, 0xb4, {0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8}}
    };
    
    /* Name string is an ISO OID */
    constexpr static uuid_t NameSpace_OID = { /* 6ba7b812-9dad-11d1-80b4-00c04fd430c8 */
            {0x6ba7b812,
             0x9dad,
             0x11d1,
             0x80, 0xb4, {0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8}}
    };
    
    /* Name string is an X.500 DN (in DER or a text output format) */
    constexpr static uuid_t NameSpace_X500 = { /* 6ba7b814-9dad-11d1-80b4-00c04fd430c8 */
            {0x6ba7b814,
             0x9dad,
             0x11d1,
             0x80, 0xb4, {0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8}}
    };
    
    class malformed_uuid_exception : public std::runtime_error
    {
        public:
            explicit malformed_uuid_exception(const std::string& error = ""): std::runtime_error(error)
            {}
    };
    
    static unsigned char hex2byte(const char* hex)
    {
        unsigned short byte = 0;
        std::istringstream iss(hex);
        iss >> std::hex >> byte;
        return byte % 0x100;
    }
    
    static std::string byte2hex(const uint8_t* data, int len)
    {
        std::stringstream ss;
        ss << std::hex;
        
        const char dash[] = {0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0};
        
        for (int i = 0; i < len; ++i)
        {
            if (dash[i])
                ss << std::dec << '-';
            ss << std::hex << std::setw(2) << std::setfill('0') << (int) data[i];
        }
        
        return ss.str();
    }
    
    static uuid_t genV5(const std::string& objectName, uuid_t namespaceUUID = NameSpace_OID)
    {
        auto new_str = std::string(reinterpret_cast<const char*>(namespaceUUID.str)) + objectName;
        const auto* c_str = reinterpret_cast<const unsigned char*>(new_str.c_str());
        unsigned char hash[SHA_DIGEST_LENGTH];
        uuid_t result{};
        
        SHA1(c_str, strlen(new_str.c_str()), hash);
        
        memcpy(result.str, hash, 16);
        
        //set high-nibble to 5 to indicate type 5
        result.str[6] &= 0x0F;
        result.str[6] |= 0x50;
        
        //set upper two bits to "10"
        result.str[8] &= 0x3F;
        result.str[8] |= 0x80;
        
        return result;
    }
    
    static std::string toString(uuid_t uuid)
    {
        return byte2hex(uuid.str, 16);
    }
    
    static uuid_t toUUID(const std::string& str)
    {
        if (str.empty())
            throw malformed_uuid_exception("expected at least 32 characters!");
        if (str.size() > 36)
            throw malformed_uuid_exception("UUID cannot contain more then 128 bits of information!");
        
        uuid_t uuid{};
        
        std::string data = str;
        
        if (data.size() == 36)
            blt::string::replaceAll(data, "-", "");
        
        if (data.size() == 32)
        {
            char cpy[2];
            for (size_t i = 0; i < data.size(); i += 2)
            {
                cpy[0] = data[i];
                cpy[1] = data[i + 1];
                uuid.str[i / 2] = hex2byte(cpy);
            }
        } else
            throw malformed_uuid_exception("UUID is expected as a string of bytes xxxxxxxx or in uuid format 8-4-4-4-12");
        
        return uuid;
    }
    
    static uuid_t genV4()
    {
        std::random_device rd;
        std::seed_seq seed{rd(), rd(), rd(), rd()};
        std::mt19937_64 gen(seed);
        std::uniform_int_distribution<int> dis(0, 15);
        std::uniform_int_distribution<> dis2(8, 11);
        
        std::stringstream ss;
        int i;
        ss << std::hex;
        for (i = 0; i < 8; i++)
        {
            ss << dis(gen);
        }
        ss << "-";
        for (i = 0; i < 4; i++)
        {
            ss << dis(gen);
        }
        ss << "-4";
        for (i = 0; i < 3; i++)
        {
            ss << dis(gen);
        }
        ss << "-";
        ss << dis2(gen);
        for (i = 0; i < 3; i++)
        {
            ss << dis(gen);
        }
        ss << "-";
        for (i = 0; i < 12; i++)
        {
            ss << dis(gen);
        };
        
        return toUUID(ss.str());
    }
    
    
}

#endif //BLT_UUID_H
