CREATE INDEX derived_dependents_by_dependency
  ON dependents_index (dependency_object_id, dependency_component_index);
