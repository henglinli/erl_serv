// include
include "account.thrift"
include "profile.thrift"
include "captcha.thrift"
include "nakama.thrift"
include "date.thrift"
// namesapce
namespace cpp user
namespace java user
// lock for field
struct Lock {
  1: bool record = true,
}
// user
struct User {
  // account
  1: required account.Account account,
  // profile
  2: optional profile.Profile profile,
  // date records
  3: optional date.Record record,
  // date mailbox
  4: optional date.Mailbox mailbox,
  // lock for field
  5: required Lock lock,
}
// error
exception Error {
  // error type
  1: required string type,
  // error message
  2: required string message,
}
// error type
const string RegisterError = "Register error",

// user service 
service Serv {
  // common
  ///////////////////////////////////////////////////
  // ping
  void Ping(),
  // service version
  string GetVersion(),
  //////////////////////////////////////////////////
  // register
  captcha.Captcha Register(1: User user) throws (1: Error e),
  // veryfy register and replay user
  User VeryfyRegister(1: string code) throws (1: Error e),
  // veryfy register only
  void VeryfyRegisterOnly(1: string code) throws (1: Error e),
  //////////////////////////////////////////////////
  // login 
  void Login(1: account.Account account) throws (1: Error e),
  //////////////////////////////////////////////////
  // get account
  account.Account GetAcount() throws (1: Error e),
  // update account
  void UpdateAccount(1:account.Account account, 2:bool veryfy) throws (1: Error e),
  // verfy update
  void VeryfyUpdateAccount(1: string code) throws (1: Error e),  
  //////////////////////////////////////////////////
  // get friends
  list<nakama.Friend> GetFriends() throws (1: Error e),
  // add frinds
  // update friends
  void UpdateFriends(1: list<profile.Platform> platforms) throws (1: Error e),
  //////////////////////////////////////////////////
  // get info
  profile.Info GetInfo() throws (1: Error e),
  // update info
  void UpdateInfo(1:profile.Info info, 2:bool veryfy) throws (1: Error e),
  // veryfy info
  void VeryfyUpdateInfo(1: string code) throws (1: Error e),
  //////////////////////////////////////////////////
  // Date mailbox
  // request
  void Request(1: date.RequestTo to) throws (1: Error e),
  // check request
  list<date.RequestFrom> CheckRequest() throws (1: Error e),
  // response
  void Response(1: date.ResponseTo to) throws (1: Error e),
  // check response
  list<date.RequestFrom> CheckResponse() throws (1: Error e),
  //////////////////////////////////////////////////
  // Date record
  // get date record
  date.Record GetDateRecord() throws (1: Error e),
  // lock record, cannot update
  void LockRecord() throws (1: Error e),
  // unlock record
  void UnlockRecord() throws (1: Error e),
  // update profile
  void UpdateDateRecord(1:date.Record record, 2:bool veryfy) throws (1: Error e),
  // veryfy update
  void VeryfyUpdateDateRecord(1: string code) throws (1: Error e),
}