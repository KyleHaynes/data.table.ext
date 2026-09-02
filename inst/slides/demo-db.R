# A small, made-up address database used by the intro slides
# (inst/slides/duckdt-intro.qmd) and by the screenshot script beside it.
#
# Four tables, with real primary and foreign keys declared, so duckdt_erd()
# has something to draw: a tidy reference list (states -> localities ->
# addresses) plus a messy inbound client file pointing at it.

demo_address_db <- function(path = ":memory:") {
  if (path != ":memory:" && file.exists(path)) unlink(path)
  con <- duckdt::duckdt_connect(path, quiet = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE states (
      state_id INTEGER PRIMARY KEY,
      state    VARCHAR
    )")
  DBI::dbExecute(con, "
    CREATE TABLE localities (
      locality_id INTEGER PRIMARY KEY,
      locality    VARCHAR,
      postcode    VARCHAR,
      state_id    INTEGER REFERENCES states(state_id)
    )")
  DBI::dbExecute(con, "
    CREATE TABLE addresses (
      address_id  INTEGER PRIMARY KEY,
      number      VARCHAR,
      street      VARCHAR,
      street_type VARCHAR,
      locality_id INTEGER REFERENCES localities(locality_id),
      latitude    DOUBLE,
      longitude   DOUBLE
    )")
  DBI::dbExecute(con, "
    CREATE TABLE client_addresses (
      client_id   INTEGER PRIMARY KEY,
      raw_address VARCHAR,
      address_id  INTEGER REFERENCES addresses(address_id)
    )")

  DBI::dbExecute(con, "INSERT INTO states VALUES
    (1, 'WA'), (2, 'NSW')")

  DBI::dbExecute(con, "INSERT INTO localities VALUES
    (1, 'PERTH',      '6000', 1),
    (2, 'FREMANTLE',  '6160', 1),
    (3, 'SUBIACO',    '6008', 1),
    (4, 'SYDNEY',     '2000', 2)")

  DBI::dbExecute(con, "INSERT INTO addresses VALUES
    (101, '12',  'ST GEORGES', 'TCE',  1, -31.9550, 115.8600),
    (102, '200', 'HAY',        'ST',   1, -31.9540, 115.8580),
    (103, '5',   'HIGH',       'ST',   2, -32.0560, 115.7480),
    (104, '17A', 'SOUTH',      'TCE',  2, -32.0570, 115.7490),
    (105, '3',   'ROKEBY',     'RD',   3, -31.9490, 115.8250),
    (106, '88',  'HAY',        'ST',   3, -31.9500, 115.8260),
    (107, '1',   'MARTIN',     'PL',   4, -33.8680, 151.2090),
    (108, '44',  'PITT',       'ST',   4, -33.8650, 151.2070)")

  DBI::dbExecute(con, "INSERT INTO client_addresses VALUES
    (1, '12 st georges terrace, perth wa 6000',  101),
    (2, '200 Hay St PERTH 6000',                 102),
    (3, 'unit 2, 5 high street fremantle',       103),
    (4, '17a South Tce, Fremantle WA',           104),
    (5, '3 Rokeby Road, Subiaco',                105),
    (6, '88 hay st subiaco wa 6008',             106),
    (7, 'One Martin Place Sydney',               NULL),
    (8, '44 pitt st, sydney nsw 2000',           108)")

  con
}
