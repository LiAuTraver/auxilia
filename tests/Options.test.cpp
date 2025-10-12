#include <gtest/gtest.h>
#include <sstream>

#include "accat/auxilia/details/config.hpp"
#include "accat/auxilia/details/program_options.hpp"

using namespace accat::auxilia::program_options;

TEST(Options, Parsing) {
  auto parser = Local("test", "1.0");
  parser.add_option("my-help", "--my-help", "-m", "Show help message").nargs(0);
  parser.add_option("output", "--output", "-o", "Output file").nargs('1');

  std::vector<std::string_view> args = {"--my-help", "-o", "file.txt"};
  EXPECT_TRUE(parser.parse(args));
  EXPECT_EQ(parser.error(), 0);

  auto help_opt = parser.get_option("my-help");
  ASSERT_NE(help_opt, nullptr);
  EXPECT_FALSE(help_opt->values().empty());
  EXPECT_EQ(help_opt->values()[0], "true");

  auto output_opt = parser.get_option("output");
  ASSERT_NE(output_opt, nullptr);
  EXPECT_FALSE(output_opt->values().empty());
  EXPECT_EQ(output_opt->values()[0], "file.txt");
}

TEST(Options, Required) {
  auto parser = Local("test", "1.0");
  parser.add_option("input", "--input", "-i", "Input file")
      .nargs('1')
      .required();

  std::vector<std::string_view> args = {};
  EXPECT_FALSE(parser.parse(args));
  EXPECT_EQ(parser.error(), 1);
  EXPECT_EQ(parser.error_messages()[0], "Required option --input is missing.");
}

TEST(Options, Positional) {
  auto parser = Local("test", "1.0");
  parser.add_option("other", "--other", "-t").nargs(0);

  std::vector<std::string_view> args = {"pos1", "-t", "pos2"};
  EXPECT_TRUE(parser.parse(args));
  EXPECT_EQ(parser.error(), 0);

  auto positional_args = parser.positional_arguments();
  ASSERT_EQ(positional_args.size(), 2);
  EXPECT_EQ(positional_args[0], "pos1");
  EXPECT_EQ(positional_args[1], "pos2");

  auto other_opt = parser.get_option("other");
  ASSERT_NE(other_opt, nullptr);
  EXPECT_EQ(other_opt->values().size(), 1);
}

TEST(Options, Default) {
  auto p1 = Local("test1", "1.0");
  p1.add_option("level", "--level", "-l").nargs('1').default_value("info");
  std::vector<std::string_view> args1 = {};
  EXPECT_TRUE(p1.parse(args1));
  auto level_opt1 = p1.get_option("level");
  ASSERT_NE(level_opt1, nullptr);
  ASSERT_EQ(level_opt1->values().size(), 1);
  EXPECT_EQ(level_opt1->values()[0], "info");

  auto p2 = Local("test2", "1.0");
  p2.add_option("level", "--level", "-l").nargs('1').default_value("info");
  std::vector<std::string_view> args2 = {"--level", "debug"};
  EXPECT_TRUE(p2.parse(args2));
  auto level_opt2 = p2.get_option("level");
  ASSERT_NE(level_opt2, nullptr);
  ASSERT_EQ(level_opt2->values().size(), 1);
  EXPECT_EQ(level_opt2->values()[0], "debug");
}

TEST(Options, HelpAndVersion) {
  auto parser = Local("my_app", "1.0.0");
  parser.add_option("input", "--input", "-i", "Input file").nargs('1');

  std::ostringstream V;
  parser.version(V);
  std::string version_msg = V.str();
  EXPECT_EQ(version_msg, "my_app version 1.0.0\n");

  std::ostringstream H;
  parser.help(H);
  std::string help_msg = H.str();
  EXPECT_NE(help_msg.find("Usage: my_app [options]"), std::string::npos);
  EXPECT_NE(help_msg.find("-h, --help"), std::string::npos);
  EXPECT_NE(help_msg.find("-v, --version"), std::string::npos);
  EXPECT_NE(help_msg.find("-i, --input <value>"), std::string::npos);
  EXPECT_NE(help_msg.find("Input file"), std::string::npos);

  std::vector<std::string_view> help_args = {"--help"};
  EXPECT_TRUE(parser.parse(help_args));
  EXPECT_EQ(parser.error(), 0);

  auto p2 = Local("my_app", "1.0.0");
  std::vector<std::string_view> version_args = {"--version"};
  EXPECT_TRUE(p2.parse(version_args));
  EXPECT_EQ(p2.error(), 0);
}

TEST(Options, Reserved) {
  auto parser = Local("test", "1.0");
  parser.add_option("help", "--my-help", "-h");
  EXPECT_EQ(parser.error(), 1);
  EXPECT_NE(parser.error_messages()[0].find("'help' is reserved"),
            std::string::npos);

  auto p2 = Local("test2", "1.0");
  p2.add_option("version", "--my-version", "-v");
  EXPECT_EQ(p2.error(), 1);
  EXPECT_NE(p2.error_messages()[0].find("'version' is reserved"),
            std::string::npos);
}

TEST(Options, Conflicting) {
  auto parser = Local("test", "1.0");
  parser.add_option("opt1", "--option", "-o");
  parser.add_option("opt2", "--option", "-x"); // Conflicting fullname
  EXPECT_EQ(parser.error(), 1);
  EXPECT_NE(parser.error_messages()[0].find("conflicts with existing option"),
            std::string::npos);

  auto p2 = Local("test2", "1.0");
  p2.add_option("opt1", "--option1", "-o");
  p2.add_option("opt2", "--option2", "-o"); // Conflicting shortname
  EXPECT_EQ(p2.error(), 1);
  EXPECT_NE(p2.error_messages()[0].find("conflicts with existing option"),
            std::string::npos);
}
