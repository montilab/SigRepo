# SigRepo 1.0.0

- Documentation: <https://montilab.github.io/SigRepo/>
- GitHub: <https://github.com/montilab/SigRepo/>
- Uni-directional signatures may omit scores: the four
  `add*SignatureSet()` functions no longer require a non-empty `score`,
  which is stored as NULL and comes back as NA. `addSignature()` now
  rolls back and errors if the feature step stored no rows, instead of
  returning a signature id for an empty signature. Needs OmicSignature
  &gt;= 1.4.1, which keeps NA-score rows (#258,
  montilab/SigRepo\_Server#89).
- `addUser()` and `updateUser()` now grant privileges on the database
  the connection is on instead of a hard-coded `sigrepo` schema, and
  `checkDBTable()` reads `SHOW TABLES` by position, so seeding and
  lookups work on a database with any name (#245,
  montilab/SigRepo\_Server#130).
