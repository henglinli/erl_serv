//
namespace cpp account
namespace java account
// type
enum AccountType {
  kSelf = 0,
  kSina = 1,
  kTencent = 2,
  kBaidu = 3,
  kRenren = 4,
  kTaobao = 5,
  kMomo = 6,
  kDouban = 7,
  kWechat = 8,
  kEnd = 9,
}
// Account
struct Account {
  1: required AccountType type = AccountType.kSelf,
  // name
  2: required string name = "",
  // token
  3: required string token = "",
  // master email
  4: optional string email,
}