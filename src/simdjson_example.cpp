
#define STRICT_R_HEADERS
#include <Rcpp.h>

#if __cplusplus >= 201703L
#include "simdjson.h"
#include "simdjson.cpp"
#endif

// [[Rcpp::export(.validateJSON)]]
bool validateJSON(const std::string filename) {
#if __cplusplus >= 201703L
  simdjson::dom::parser parser;
  auto [doc, error] = parser.load(filename.c_str()); // do the parsing
  if ( error ) {
    // something went wrong
    Rcpp::stop(error_message(error));
  }
  return true;
#else
  Rcpp::stop("The package 'SystemRequirements:' state a need for a C++17 compiler.");
  return true; // not reached
#endif
}

// [[Rcpp::export]]
void parseExample() {
  // from the blogpost announcing version 0.3
  // https://lemire.me/blog/2020/03/31/
  // we-released-simdjson-0-3-the-fastest-json-parser-in-the-world-is-even-better/
  auto cars_json = R"( [
    { "make": "Toyota", "model": "Camry",  "year": 2018,
         "tire_pressure": [ 40.1, 39.9 ] },
    { "make": "Kia",    "model": "Soul",   "year": 2012,
         "tire_pressure": [ 30.1, 31.0 ] },
    { "make": "Toyota", "model": "Tercel", "year": 1999,
         "tire_pressure": [ 29.8, 30.0 ] }
  ] )"_padded;
  simdjson::dom::parser parser;
  simdjson::dom::array cars = parser.parse(cars_json).get<dom::array>();

  // Iterating through an array of objects
  for (simdjson::dom::object car : cars) {
    // Accessing a field by name
    Rcpp::Rcout << "Make/Model: " << car["make"]
                << "/" << car["model"] << std::endl;

    // Casting a JSON element to an integer
    uint64_t year = car["year"];
    Rcpp::Rcout << "- This car is " << 2020 - year
                << " years old." << std::endl;

    // Iterating through an array of floats
    double total_tire_pressure = 0;
    for (double tire_pressure : car["tire_pressure"]) {
      total_tire_pressure += tire_pressure;
    }
    Rcpp::Rcout << "- Average tire pressure: "
                << (total_tire_pressure / 2) << std::endl;

    // Writing out all the information about the car
    for (auto [key, value] : car) {
      Rcpp::Rcout << "- " << key << ": " << value << std::endl;
    }
  }
}

// [[Rcpp::export(.cppVersion)]]
int cppVersion() {
  return __cplusplus;
}

// [[Rcpp::export(.unsupportedArchitecture)]]
bool unsupportedArchitecture() {
  return (simdjson::active_implementation->name() == "unsupported");
}
