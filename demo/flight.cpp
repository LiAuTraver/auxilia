#include <iostream>
#include <memory>
#include <unordered_map>
#include <string>
#include <optional>

#include <accat/auxilia/details/Trie.hpp>
using namespace accat::auxilia;

int main() {
  Trie<std::string, int> trie;

  // Insert test data
  trie.insert_or_assign("apple", 1);
  trie.insert_or_assign("app", 2);
  trie.insert_or_assign("banana", 3);
  trie.insert_or_assign("band", 4);
  trie.insert_or_assign("bandit", 5);

  // Test lookups
  const auto print_lookup = [&](const std::string &key) {
    const auto v = trie.find(key);
    if (v)
      std::cout << "FOUND:  " << key << " = " << v->value() << "\n";
    else
      std::cout << "MISSING: " << key << "\n";
  };

  print_lookup("apple");
  print_lookup("app");
  print_lookup("banana");
  print_lookup("band");
  print_lookup("bandit");
  print_lookup("ban");     // should be missing
  print_lookup("bandana"); // missing

  std::cout << "\nErasing 'band'...\n";
  trie.erase("band");

  print_lookup("band");   // removed
  print_lookup("bandit"); // must still exist

  std::cout << "\nErasing 'apple'...\n";
  trie.erase("apple");

  print_lookup("apple"); // removed
  print_lookup("app");   // must still exist

  return 0;
}
