/*
 * Created by Brett on 31/03/23.
 * Licensed under GNU General Public License V3.0
 * See LICENSE file for license detail
 */

#ifndef BLT_HASH_MAP_H
#define BLT_HASH_MAP_H

namespace blt
{

//    template<typename K, typename V, typename Hash = std::hash<K>, typename Eq = std::equal_to<K>>
//    class hashmap
//    {
//        private:
//
//        public:
//
//    };

}

#ifndef HASHMAP
    #if defined __has_include && __has_include(<parallel_hashmap/phmap.h>)
        
        #include <parallel_hashmap/phmap.h>
        #include <parallel_hashmap/phmap_fwd_decl.h>

namespace blt
{
    template<class K, class V,
            class Hash  = phmap::priv::hash_default_hash <K>,
            class Eq    = phmap::priv::hash_default_eq <K>,
            class Alloc = phmap::priv::Allocator <phmap::priv::Pair<const K, V>>>
    using hashmap_t = phmap::flat_hash_map<K, V, Hash, Eq, Alloc>;
    template<class T,
            class Hash  = phmap::priv::hash_default_hash <T>,
            class Eq    = phmap::priv::hash_default_eq <T>,
            class Alloc = phmap::priv::Allocator <T>>
    using hashset_t = phmap::flat_hash_set<T, Hash, Eq, Alloc>;
}
    #else
        #include <unordered_map>
        #include <unordered_set>
namespace blt {

    template<typename K, typename V,
        typename Hash = std::hash<K>,
        typename Eq = std::equal_to<K>,
        typename Alloc = std::allocator<std::pair<const K, V>>>
    using hashmap_t = std::unordered_map<K, V, Hash, Eq, Alloc>;
    
    template<typename K,
        typename Hash = std::hash<K>,
        typename Eq = std::equal_to<K>,
        typename Alloc = std::allocator<K>>
    using hashset_t = std::unordered_set<K, Hash, Eq, Alloc>;
}
    #endif
#endif

#endif //BLT_HASH_MAP_H
