#include <gtest/gtest.h>

#include <accat/auxilia/details/Status.hpp>

using namespace accat::auxilia;

TEST(Status, DefaultConstruction) {
  Status s;
  EXPECT_EQ(s.code(), Status::kOk);
  EXPECT_TRUE(s.message().empty());
  EXPECT_TRUE(s.ok());
}

TEST(Status, InitializerListReturn) {
  auto result = []() -> Status { return {}; }();
  Status s;
  EXPECT_EQ(result.code(), s.code());
}

TEST(Status, FactoryOkFunction) {
  auto result = OkStatus("Ok!");
  EXPECT_EQ(result.code(), Status::kOk);
  EXPECT_EQ(result.message(), "Ok!");
  EXPECT_TRUE(result.ok());
}

TEST(Status, FactoryErrorFunction) {
  auto result = InvalidArgumentError("Invalid argument!");
  EXPECT_EQ(result.code(), Status::kInvalidArgument);
  EXPECT_EQ(result.message(), "Invalid argument!");
  EXPECT_FALSE(result.ok());
}

TEST(Status, CopyConstruction) {
  Status original = NotFoundError("Not Found!");
  Status copy = original;
  EXPECT_EQ(copy.code(), Status::kNotFound);
  EXPECT_EQ(copy.message(), "Not Found!");
  EXPECT_FALSE(copy.ok());
}

TEST(Status, MoveConstruction) {
  Status original = UnimplementedError("Unimplemented!");
  Status moved = std::move(original);
  EXPECT_EQ(moved.code(), Status::kUnimplemented);
  EXPECT_EQ(moved.message(), "Unimplemented!");
  EXPECT_FALSE(moved.ok());
  EXPECT_EQ(original.code(), Status::kMovedFrom);
}

TEST(Status, CopyAssignment) {
  Status original = PermissionDeniedError("Permission Denied!");
  Status copy;
  copy = original;
  EXPECT_EQ(copy.code(), Status::kPermissionDenied);
  EXPECT_EQ(copy.message(), "Permission Denied!");
  EXPECT_FALSE(copy.ok());
}

TEST(Status, MoveAssignment) {
  Status original = AlreadyExistsError("Already exists!");
  Status moved;
  moved = std::move(original);
  EXPECT_EQ(moved.code(), Status::kAlreadyExists);
  EXPECT_EQ(moved.message(), "Already exists!");
  EXPECT_FALSE(moved.ok());
}

TEST(Status, Streaming) {
  Status s = FailedPreconditionError("Failed precondition!");
  std::stringstream ss;
  ss << s;
  EXPECT_EQ(ss.str(), "Status 9: Failed precondition!");
}