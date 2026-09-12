# Browser property columns

Instance pickers now send declared instance-variable values as named display
columns. Inpick renders them in the candidate pane as a table: object identity
is the first column and each declared property becomes a header. The complete
object JSON remains attached to the selected record, and the inspector remains
the place for deeply nested data and editing.

The initial ordering is the order in the class's `instanceVars:` declaration.
That is useful, deterministic, and does not require each browser to invent a
presentation policy.

## Future idea: explicit display order

If declaration order proves insufficient, add an opt-in class pragma rather
than a picker-specific configuration file, for example:

```trashtalk
pragma: browserColumns: #(name status updatedAt)
```

The compiler could retain this as class presentation metadata. Instance record
generation would use the listed fields first, append unlisted declared fields,
and ignore unknown names with a compile warning. The pragma should not alter
persistence, object layout, or inspector data. It is deliberately only an idea:
the current implementation has no pragma or new language semantics.
