open Types

val parse_service_schema : string -> (service_schema list, string) result
val validate_service_schema : service_schema list -> (unit, string) result
