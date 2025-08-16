/*
 * Created by Brett on 27/01/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_NBT_H
#define BLT_NBT_H

#include <utility>
#include <cstring>
#include <type_traits>
#include <unordered_map>
#include <algorithm>

#include "blt/format/format.h"
#include "blt/fs/filesystem.h"
#include "blt/logging/logging.h"
#include "blt/std/memory.h"

#include <blt/std/hashmap.h>

namespace blt::nbt {
    
    void writeUTF8String(blt::fs::writer_t& stream, const std::string& str);
    
    std::string readUTF8String(blt::fs::reader_t& stream);
    
    template<typename T>
    inline static void writeData(blt::fs::writer_t& out, const T& d){
        char data[sizeof(T)];
        mem::toBytes(d, data);
        out.write(data, sizeof(T));
    }
    
    template<typename T>
    inline static void readData(blt::fs::reader_t& in, T& d) {
        char data[sizeof(T)];
        in.read(data, sizeof(T));
        mem::fromBytes(data, &d);
    }
    
    enum class nbt_tag : char {
        END = 0,
        BYTE = 1,
        SHORT = 2,
        INT = 3,
        LONG = 4,
        FLOAT = 5,
        DOUBLE = 6,
        BYTE_ARRAY = 7,
        STRING = 8,
        LIST = 9,
        COMPOUND = 10,
        INT_ARRAY = 11,
        LONG_ARRAY = 12
    };
    
    class tag_t {
        protected:
            nbt_tag type;
            std::string name;
        public:
            explicit tag_t(nbt_tag type): type(type) {};
            explicit tag_t(nbt_tag type, std::string name): type(type), name(std::move(name)) {}
            virtual void writePayload(blt::fs::writer_t& out) = 0;
            virtual void readPayload(blt::fs::reader_t& in) = 0;
            void writeName(blt::fs::writer_t& out) {
                writeUTF8String(out, name);
            }
            void readName(blt::fs::reader_t& in) {
                name = readUTF8String(in);
            }
            [[nodiscard]] inline nbt_tag getType() const {
                return type;
            }
            [[nodiscard]] inline const std::string& getName() const {
                return name;
            }
            virtual ~tag_t() = default;
    };
    
    template<typename T>
    class tag : public tag_t {
        protected:
            T t;
        public:
            explicit tag(nbt_tag type): tag_t(type) {}
            tag(nbt_tag type, std::string name, T t): tag_t(type, std::move(name)), t(std::move(t)) {}
            void writePayload(blt::fs::writer_t& out) override {
                if constexpr(std::is_arithmetic<T>::value)
                    writeData(out, t);
            }
            void readPayload(blt::fs::reader_t& in) override {
                if constexpr(std::is_arithmetic<T>::value)
                    readData(in, t);
            }
            [[nodiscard]] inline const T& get() const {return t;}
            inline T& get() {return t;}
            ~tag() override = default;
    };
    
    class tag_end : public tag<char> {
        public:
            void writePayload(blt::fs::writer_t&) final {}
            // nothing to read
            void readPayload(blt::fs::reader_t&) final {}
    };
    
    class tag_byte : public tag<int8_t> {
        public:
            tag_byte(): tag(nbt_tag::BYTE) {}
            tag_byte(const std::string& name, int8_t b): tag(nbt_tag::BYTE, name, b) {}
    };
    
    class tag_short : public tag<int16_t> {
        public:
            tag_short(): tag(nbt_tag::SHORT) {}
            tag_short(const std::string& name, int16_t s): tag(nbt_tag::SHORT, name, s) {}
    };
    
    class tag_int : public tag<int32_t> {
        public:
            tag_int(): tag(nbt_tag::INT) {}
            tag_int(const std::string& name, int32_t i): tag(nbt_tag::INT, name, i) {}
    };
    
    class tag_long : public tag<int64_t> {
        public:
            tag_long(): tag(nbt_tag::LONG) {}
            tag_long(const std::string& name, int64_t l): tag(nbt_tag::LONG, name, l) {}
    };
    
    class tag_float : public tag<float> {
        public:
            tag_float(): tag(nbt_tag::FLOAT) {}
            tag_float(const std::string& name, float f): tag(nbt_tag::FLOAT, name, f) {}
    };
    
    class tag_double : public tag<double> {
        public:
            tag_double(): tag(nbt_tag::DOUBLE) {}
            tag_double(const std::string& name, double d): tag(nbt_tag::DOUBLE, name, d) {}
    };
    
    class tag_byte_array : public tag<std::vector<int8_t>> {
        public:
            tag_byte_array(): tag(nbt_tag::BYTE_ARRAY) {}
            tag_byte_array(const std::string& name, const std::vector<int8_t>& v): tag(nbt_tag::BYTE_ARRAY, name, v) {}
            void writePayload(blt::fs::writer_t& out) final {
                auto length = (int32_t) t.size();
                writeData(out, length);
                // TODO on the writer (remove need for cast + more std::fstream functions)
                out.write(reinterpret_cast<char*>(t.data()), length);
            }
            void readPayload(blt::fs::reader_t& in) final {
                int32_t length;
                readData(in, length);
                t.reserve(length);
                in.read(reinterpret_cast<char*>(t.data()), length);
            }
    };
    
    class tag_string : public tag<std::string> {
        public:
            tag_string(): tag(nbt_tag::STRING) {}
            tag_string(const std::string& name, const std::string& s): tag(nbt_tag::STRING, name, s) {}
            void writePayload(blt::fs::writer_t& out) final {
                writeUTF8String(out, t);
            }
            void readPayload(blt::fs::reader_t& in) final {
                t = readUTF8String(in);
            }
    };
    
    class tag_int_array : public tag<std::vector<int32_t>> {
        public:
            tag_int_array(): tag(nbt_tag::INT_ARRAY) {}
            tag_int_array(const std::string& name, const std::vector<int32_t>& v): tag(nbt_tag::INT_ARRAY, name, v) {}
            void writePayload(blt::fs::writer_t& out) final {
                auto length = (int32_t) t.size();
                writeData(out, length);
                for (int i = 0; i < length; i++)
                    writeData(out, t[i]);
            }
            void readPayload(blt::fs::reader_t& in) final {
                int32_t length;
                readData(in, length);
                t.reserve(length);
                for (int i = 0; i < length; i++)
                    readData(in, t[i]);
            }
    };
    
    class tag_long_array : public tag<std::vector<int64_t>> {
        public:
            tag_long_array(): tag(nbt_tag::LONG_ARRAY) {}
            tag_long_array(const std::string& name, const std::vector<int64_t>& v): tag(nbt_tag::LONG_ARRAY, name, v) {}
            void writePayload(blt::fs::writer_t& out) final {
                auto length = (int32_t) t.size();
                writeData(out, length);
                for (int i = 0; i < length; i++)
                    writeData(out, t[i]);
            }
            void readPayload(blt::fs::reader_t& in) final {
                int32_t length;
                readData(in, length);
                t.reserve(length);
                for (int i = 0; i < length; i++)
                    readData(in, t[i]);
            }
    };
    
#define BLT_NBT_POPULATE_VEC(type, vec, length) for (int i = 0; i < length; i++) vec.push_back(type);
    
    namespace _internal_ {
        // EVIL HACK
        static tag_t* newCompound();
        static tag_t* newList();
        static tag_t* toType(char id){
            switch ((nbt_tag) id) {
                case nbt_tag::END:
                    return nullptr;
                    break;
                case nbt_tag::BYTE:
                    return new blt::nbt::tag_byte;
                case nbt_tag::SHORT:
                    return new blt::nbt::tag_short;
                case nbt_tag::INT:
                    return new blt::nbt::tag_int;
                case nbt_tag::LONG:
                    return new blt::nbt::tag_long;
                case nbt_tag::FLOAT:
                    return new blt::nbt::tag_float;
                case nbt_tag::DOUBLE:
                    return new blt::nbt::tag_double;
                case nbt_tag::BYTE_ARRAY:
                    return new blt::nbt::tag_byte_array;
                case nbt_tag::STRING:
                    return new blt::nbt::tag_string;
                case nbt_tag::LIST:
                    return _internal_::newList();
                case nbt_tag::COMPOUND:
                    return _internal_::newCompound();
                case nbt_tag::INT_ARRAY:
                    return new blt::nbt::tag_int_array;
                case nbt_tag::LONG_ARRAY:
                    return new blt::nbt::tag_long_array;
            }
            BLT_WARN("Tag Type not found!");
            return nullptr;
        }
        static hashmap_t<std::string, tag_t*> toHashmap(const std::vector<tag_t*>& v){
            hashmap_t<std::string, tag_t*> tags;
            for (const auto& t : v)
                tags[t->getName()] = t;
            return tags;
        }
    }
    
    class tag_list : public tag<std::vector<tag_t*>> {
        public:
            tag_list(): tag(nbt_tag::LIST) {}
            tag_list(const std::string& name, const std::vector<tag_t*>& v): tag(nbt_tag::LIST, name, v) {}
            void writePayload(blt::fs::writer_t& out) final {
                if (t.empty())
                    writeData(out, (char)nbt_tag::END);
                else
                    writeData(out, (char)t[0]->getType());
                auto length = (int32_t) t.size();
                writeData(out, length);
                for (const auto& v : t)
                    v->writePayload(out);
            }
            
            void readPayload(blt::fs::reader_t& in) final {
                char id;
                int32_t length;
                readData(in, id);
                readData(in, length);
                if (length == 0 || id == 0)
                    return;
                for (int i = 0; i < length; i++) {
                    auto v = _internal_::toType(id);
                    v->readPayload(in);
                    t.push_back(v);
                }
            }
            
            inline void put(tag_t* tag) {
                t.push_back(tag);
            }
            
            inline tag_t*& operator[](size_t i){
                return t[i];
            }
            
            template<typename T>
            [[nodiscard]] T* getTag(size_t i){
                if constexpr (!std::is_base_of<tag_t, T>::value) {
                    static_assert("WARNING: provided type isn't of type tag. Cannot cast expression!");
                    BLT_WARN("You have requested an invalid type. Please use types of tag_t when using getTag");
                    return nullptr;
                }
                auto& tag = t[i];
                T t;
                if (tag->getType() != t.getType()) {
                    BLT_WARN("Expected tag of type {:d} but got tag of type {:d}", (char)t.getType(), (char)tag->getType());
                    throw std::runtime_error("Requested Tag does not match stored type!");
                }
                return dynamic_cast<T*>(tag);
            }
            
            [[nodiscard]] inline size_t size() const {
                return t.size();
            }
            
            ~tag_list() override {
                for (auto* p : t)
                    delete p;
            }
    };
    
    class tag_compound : public tag<hashmap_t<std::string, tag_t*>> {
        public:
            tag_compound(): tag(nbt_tag::COMPOUND) {}
            tag_compound(const std::string& name, const std::vector<tag_t*>& v): tag(nbt_tag::COMPOUND, name, _internal_::toHashmap(v)) {}
            tag_compound(const std::string& name, const std::initializer_list<tag_t*>& v): tag(nbt_tag::COMPOUND, name, _internal_::toHashmap(v)) {}
            tag_compound(const std::string& name, const hashmap_t<std::string, tag_t*>& v): tag(nbt_tag::COMPOUND, name, v) {}
            
            inline void put(tag_t* tag) {
                t[tag->getName()] = tag;
            }
            
            template<typename T>
            [[nodiscard]] T* getTag(const std::string& name){
                if constexpr (!std::is_base_of<tag_t, T>::value) {
                    static_assert("WARNING: provided type isn't of type tag. Cannot cast expression!");
                    BLT_WARN("You have requested an invalid type. Please use types of tag_t when using getTag");
                    return nullptr;
                }
                auto& tag = t[name];
                T t;
                if (tag->getType() != t.getType()) {
                    BLT_WARN("Expected tag of type {:d} but got tag of type {:d}", (char)t.getType(), (char)tag->getType());
                    throw std::runtime_error("Requested Tag does not match stored type!");
                }
                return dynamic_cast<T*>(tag);
            }
            
            inline tag_t*& operator[](const std::string& name){
                return t[name];
            }
            
            inline void operator()(tag_t* tag){
                t[tag->getName()] = tag;
            }
            
            void writePayload(blt::fs::writer_t& out) final {
                for (const auto& v : t){
                    auto tag = v.second;
                    auto c = (char) tag->getType();
                    out.write(&c, 1);
                    tag->writeName(out);
                    tag->writePayload(out);
                }
                const char c = '\0';
                out.write(&c, 1);
            }
            void readPayload(blt::fs::reader_t& in) final {
                char type;
                while ((in.read(&type, 1), type) != (char)nbt_tag::END){
                    auto* v = _internal_::toType(type);
                    v->readName(in);
                    v->readPayload(in);
                    t[v->getName()] = v;
                }
            }
            ~tag_compound() override {
                for (auto& v : t)
                    delete v.second;
            }
    };
    
    static tag_t* _internal_::newCompound(){
        return new blt::nbt::tag_compound;
    }
    
    static tag_t* _internal_::newList() {
        return new blt::nbt::tag_list;
    }
    
    class NBTReader {
        private:
            blt::fs::reader_t& reader;
            tag_compound* root = nullptr;
        public:
            explicit NBTReader(blt::fs::reader_t& reader): reader(reader) {}
            
            void read();
            
            template<typename T>
            [[nodiscard]] T* getTag(const std::string& name){
                if constexpr (!std::is_base_of<tag_t, T>::value) {
                    static_assert("WARNING: provided type isn't of type tag. Cannot cast expression!");
                    BLT_WARN("You have requested an invalid type. Please use types of tag_t when using getTag");
                    return nullptr;
                }
                auto& tag = root->get()[name];
                T t;
                if (tag->getType() != t.getType()) {
                    BLT_WARN("Expected tag of type {:d} but got tag of type {:d}", (char)t.getType(), (char)tag->getType());
                    throw std::runtime_error("Requested Tag does not match stored type!");
                }
                return dynamic_cast<T*>(tag);
            }
            ~NBTReader() {
                delete root;
            }
    };
    
    class NBTWriter {
        private:
            blt::fs::writer_t& writer;
        public:
            explicit NBTWriter(blt::fs::writer_t& writer): writer(writer) {}
            /**
             * Write a compound tag and then DELETES the tag. If you don't wish for the memory to be freed, please use the reference version!
             * @param root root NBT tag to write, this function assumes ownership of this pointer.
             */
            void write(tag_compound* root){
                write(*root);
                delete root;
            }
            void write(tag_compound& root){
                auto c = (char)nbt_tag::COMPOUND;
                writer.write(&c, 1);
                root.writeName(writer);
                root.writePayload(writer);
            }
            ~NBTWriter() = default;
    };
    
}

#endif //BLT_NBT_H
