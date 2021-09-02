## Exercise 1

json_txt = '{
  "firstName": "John",
  "lastName": "Smith",
  "age": 25,
  "address": 
    {
      "streetAddress": "21 2nd Street",
      "city": "New York",
      "state": "NY",
      "postalCode": 10021
    },
  "phoneNumber": 
    [
      {
        "type": "home",
        "number": "212 555-1239"
      },
      {
        "type": "fax",
        "number": "646 555-4567"
      }
    ]
}'


json = list(
  firstName = "John",
  "lastName" = "Smith",
  "age" = 25,
  "address" = list(
    "streetAddress" = "21 2nd Street",
    "city" = "New York",
    "state" = "NY",
    "postalCode" = 10021
  ),
  "phoneNumber" = list(
    list(
      "type" = "home",
      "number" = "212 555-1239"
    ),
    list(
      "type" = "fax",
      "number" =  "646 555-4567"
    )
  )
)

str(json)


str( jsonlite::fromJSON(json_txt, simplifyVector = FALSE) )



## Exercise 2

report = function(x) {
  UseMethod("report")
}

report.default = function(x) {
  "This class does not have a method defined."
}


report.integer = function(x) {
  "I'm an integer!"
}

report.double = function(x) {
  "I'm a double!"
}

report.numeric = function(x) {
  "I'm a numeric!"
}

report(1L)
report(1)
report("A")

rm("report.integer")

report(1L)
report(1)

rm("report.double")

report(1L)
report(1)


class(1L)
class(1)
