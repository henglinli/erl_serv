//
include "time.thrift"
include "nakama.thrift"
include "location.thrift"
include "profile.thrift"
//
namespace cpp date
namespace java date
// type
struct DateType {
  // id
  1: required i16 id = 0,
  // name
  2: optional string name = "",
}
// date
struct Date {
  // id
  1: required i64 id = 0,
  // what
  2: required DateType what,
  // when
  3: required time.Time toki,
  // where
  4: required location.Location where,
  // how
  5: required string how = "",
  // message
  6: optional string message = "",
}
// filter
// equal
struct Equal {
  // what
  1: required string what = "",
}
// less
struct Less {
    // what
  1: required string what = "",
}
// Greater
struct Greater {
  // what
  1: required string what = "",
}
// filter
struct Filter {
  // equal
  1: optional Equal equal,
  // less
  2: optional Less less,
  // bigger
  3: optional Greater greater,
}

// public request 
struct PublicRequestTo {
  // date
  1: required Date date,
  //
  2: required Filter filter,
}
// request to
struct RequestTo {
  // date
  1: required Date date,
  // from
  2: required list<nakama.Friend> to,
}
// request from
struct RequestFrom {
  // date 
  1: required Date date,
  // to
  2: required nakama.Friend from,
}
// response
struct ResponseTo {
  // to
  1: required nakama.Friend to,
  // date id
  2: required i64 date_id,
  // ok?
  3: required bool ok,
  // why
  4: optional string why = "",
}
// response from
struct ResponseFrom {
  // to
  1: required nakama.Friend from,
  // date id
  2: required i64 date_id,
  // ok?
  3: required bool ok,
  // why
  4: optional string why = "",
}
// records
struct Record {
  // requests to
  1: required list<RequestTo> requests_to,
  // requests from
  2: required list<RequestFrom> requests_from,
  // response to
  3: required list<ResponseTo> responses_to,
  // response from
  4: required list<ResponseFrom> responses_from,
}
// mailbox
struct Mailbox {
  // requests from
  1: required list<RequestFrom> requests_from,  
  // response from
  2: required list<ResponseFrom> responses_from,
}