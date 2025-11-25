#include <accat/auxilia/auxilia.hpp>
#include <accat/auxilia/defines.hpp>
#include "accat/auxilia/utility/program_options.hpp"

using namespace accat::auxilia;
using accat::auxilia::program_options::Option;
int main(int argc, char **argv) {
  auto argParser = accat::auxilia::program_options::Local("cp", "0.1");

  argParser.add_option("nfa", " -n", "Convert input regex into a NFA").nargs(1);
  argParser.add_option("dfa", " -d", "Convert input regex into a DFA").nargs(1);
  argParser.add_option("bnf", "-b", "parse BNF syntax").nargs(1);

  contract_assert(argParser.error() == 0);
}
