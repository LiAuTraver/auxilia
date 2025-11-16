#include <accat/auxilia/details/program_options.hpp>

int main(int argc, char **argv) {
  using namespace accat::auxilia;
  using namespace accat::auxilia::program_options;
  auto parser = Global("demo", "0.1");
  parser->add_option("--input", "-i", "Input file").required().nargs('+');
  parser->add_option("--verbose", "", "Enable verbose mode").nargs(0);
  parser->add_option("--output", "-o", "Output file").nargs(1);

  if (auto res = parser->parse(argc, argv); !res) {
    println("Error parsing command line arguments:\n {}",
            fmt::join(parser->error_messages(), "; "));
    return 1;
  }

  if (auto inputOpt = parser->get_option("--input");
      inputOpt && !inputOpt->values().empty()) {
    println("Input files: {}", fmt::join(inputOpt->values(), ", "));
  }
  if (auto verboseOpt = parser->get_option("--verbose");
      verboseOpt && !verboseOpt->values().empty()) {
    Println("Verbose mode enabled");
  }
  if (auto outputOpt = parser->get_option("--output")) {
    Println("Output file: {}",
            outputOpt->value().value_or("no output file provided"));
  }
}
