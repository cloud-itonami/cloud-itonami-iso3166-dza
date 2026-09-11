(ns culture.facts
  "Country-level regional-culture catalog for Algeria (DZA) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.")

(def catalog
  "iso3 -> vector of culture entries."
  {"DZA"
   [{:culture/id "dza.dish.couscous"
     :culture/name "Couscous"
     :culture/country "DZA"
     :culture/kind :dish
     :culture/summary "Staple of the Maghrebi cuisines including Algeria; in 2020 Algeria, Mauritania, Morocco and Tunisia jointly obtained UNESCO intangible-cultural-heritage recognition for couscous knowledge and practices."
     :culture/url "https://en.wikipedia.org/wiki/Couscous"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "dza.dish.chakhchoukha"
     :culture/name "Chakhchoukha"
     :culture/country "DZA"
     :culture/kind :dish
     :culture/summary "Traditional Algerian dish of torn pieces of cooked semolina dough served in a tomato-based sauce, originating from the Chaoui people and popular in Constantine, Batna, Biskra and M'Sila."
     :culture/url "https://en.wikipedia.org/wiki/Chakhchoukha"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "dza.dish.rechta"
     :culture/name "Rechta"
     :culture/country "DZA"
     :culture/kind :dish
     :culture/summary "Dish of thin fresh artisan-cut pasta typical of Algeria and a symbolic dish of Algiers cuisine, traditionally served with chicken, chickpeas and turnips in a spiced sauce."
     :culture/url "https://en.wikipedia.org/wiki/Rechta"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "dza.dish.mahdjouba"
     :culture/name "Mahdjouba"
     :culture/country "DZA"
     :culture/kind :dish
     :culture/summary "Crepe-like semolina flatbread originating from Algeria, one of the essential dishes of Algerian street food, typically stuffed with onion, garlic, tomato and peppers."
     :culture/url "https://en.wikipedia.org/wiki/Mahdjouba"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "dza.product.deglet-nour"
     :culture/name "Deglet Nour"
     :culture/country "DZA"
     :culture/kind :product
     :culture/summary "Date palm cultivar that originated in the oasis of Tolga in Algeria; the Algerian Ministry of Agriculture reserves commercial usage of the Deglet Nour label for Algerian dates."
     :culture/url "https://en.wikipedia.org/wiki/Deglet_Nour"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "dza.festival.yennayer"
     :culture/name "Yennayer"
     :culture/country "DZA"
     :culture/kind :festival
     :culture/summary "Berber New Year celebration observed on 12 January, recognized as an official public holiday in Algeria with first official observance in 2018."
     :culture/url "https://en.wikipedia.org/wiki/Yennayer"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "dza.heritage.casbah-of-algiers"
     :culture/name "Casbah of Algiers"
     :culture/country "DZA"
     :culture/kind :heritage
     :culture/summary "Old town (medina) of Algiers founded in 944 by the Zirids, a UNESCO World Heritage Site since 1992."
     :culture/url "https://en.wikipedia.org/wiki/Casbah_of_Algiers"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "dza.heritage.timgad"
     :culture/name "Timgad"
     :culture/country "DZA"
     :culture/kind :heritage
     :culture/summary "Roman city in the Aurès Mountains of Algeria founded by Emperor Trajan around 100 AD, a UNESCO World Heritage Site since 1982."
     :culture/url "https://en.wikipedia.org/wiki/Timgad"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-dza culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "DZA"))
                 " DZA entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))
