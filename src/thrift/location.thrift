//
namespace cpp date
namespace java date
// gps location
struct GPS {
  // longitude
  1: required double longitude = 0.0,
  // latitude
  2: required double latitude = 0.0,
}
// location
struct Location {
  // name
  1: required string name,
  // gps
  2: optional GPS gps,
}