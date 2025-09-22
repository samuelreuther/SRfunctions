df <- data.frame(Kunden_ID  = c(123456, 123450, NA_integer_),
                 Zepas_ID   = c(1, 2, 3),
                 POLICEN_ID = c(999999, 111111, 222222),
                 POLICE_NR  = c("A-123456", "B-000321", NA),
                 Laufnummer = c(10L, 11L, 12L),
                 Name       = c("Michael", "Anna", NA),
                 Vorname    = c("Paul", "Xi", "Élodie"),
                 Strasse    = c("Alte Ziegelei", "Hauptstrasse", "Hans-Thoma-Str"),
                 Hausnummer = c("1", "21", "5c"),
                 Telefon    = c("+41 11 111 11 01", "077 777 77 77", NA),
                 Email      = c("asdfjklö@bluewin.ch", "asdfjklö@gmail.com", NA),
                 Betrag     = c(12.3, 45.6, 7.89),
                 stringsAsFactors = FALSE)

df_clean <- data.frame(Kunden_ID  = c(100006, 100000, NA_integer_),
                 Zepas_ID   = c(1, 2, 3),
                 POLICEN_ID = c(900009, 100001, 200002),
                 POLICE_NR  = c("A-100006", "B-000001", NA),
                 Laufnummer = c(10L, 11L, 12L),
                 Name       = c("Mxxxxxx", "Axxx", NA),
                 Vorname    = c("Pxxx", "Xx", "Éxxxxx"),
                 Strasse    = c("Axxxxxxxxxxxx", "Hxxxxxxxxxxx", "Hxxxxxxxxxxxxx"),
                 Hausnummer = c("1", "21", "5c"),
                 Telefon    = c("+xxxxxxxxxxxxxxx", "0xxxxxxxxxxxx", NA),
                 Email      = c("axxxxxxxxxxxxxxxxxx", "axxxxxxxxxxxxxxxxx", NA),
                 Betrag     = c(12.3, 45.6, 7.89),
                 stringsAsFactors = FALSE)

test_that("global test", {
  expect_equal(SR_view(df, n = 3, verbose = FALSE), df_clean)
})

rm(df, df_clean)

