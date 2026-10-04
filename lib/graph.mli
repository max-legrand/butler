open Types

val generate_graph : service_schema list -> (graph, string) result
val topological_sort : graph -> (int list, string) result

type partition =
  { prerequisites : int list
  ; services : int list
  }

val partition_helper : graph -> partition
