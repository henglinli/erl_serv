// include
include "time.thrift"
include "nakama.thrift"
include "account.thrift"
// namespace
namespace cpp profile
namespace java profile
// gender
enum Gender {
  kPrivate = 0,
  kMale = 1,
  kFemale = 2,
  kBL = 3,
  kGL = 4,
  kBLG = 5,
  kGLB = 6,
  kEnd = 7,
} 
// user info 
struct Info {
  // gender
  1: optional Gender gender = Gender.kPrivate,
  // birthday
  2: optional time.Birthday birthday,
} 
// 
struct Platform {
  // account
  1: required account.Account account,
  // friends
  2: optional list<nakama.Friend> friends, 
}
// profile
struct Profile {
  1: optional Info info,
  // friend
  2: optional list<nakama.Friend> friends,
  // other platform profiles
  3: optional list<Platform> platforms,
}
