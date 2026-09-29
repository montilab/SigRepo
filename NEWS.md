# SigRepo 1.0.0

- Documentation: <https://montilab.github.io/SigRepo/>
- GitHub: <https://github.com/montilab/SigRepo/>
- `addUser()` and `updateUser()` now grant privileges on the database
  the connection is on instead of a hard-coded `sigrepo` schema, and
  `checkDBTable()` reads `SHOW TABLES` by position, so seeding and
  lookups work on a database with any name (#245,
  montilab/SigRepo\_Server#130).
