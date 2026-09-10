(ns marketentry.facts "Algeria market-entry catalog.")
(def catalog
  {"DZA" {:name "Algeria"
          :owner-authority "ARMP (Autorité de Régulation des Marchés Publics) / e-procurement"
          :legal-basis "Code des marchés publics (Décret présidentiel n° 15-247 du 16 septembre 2015, modifié par le décret n° 21-219)"
          :national-spec "e-procurement + CNRC/NIF"
          :provenance "https://www.marches-publics.gov.dz/"
          :required-evidence ["CNRC/NIF record" "e-procurement registration record" "Commercial registry extract" "Authorized-representative record"]
          :rep-owner-authority "contracting authorities / ARMP"
          :rep-legal-basis "Algerian legal entity registration typically required for public awards"
          :rep-provenance "https://www.marches-publics.gov.dz/"
          :corporate-number-owner-authority "CNRC / DGI"
          :corporate-number-legal-basis "NIF / commercial registration"
          :corporate-number-provenance "https://www.cnrc.dz/"}
   "USA" {:name "United States" :owner-authority "GSA/SAM.gov" :legal-basis "FAR" :national-spec "SAM.gov" :provenance "https://sam.gov/"
          :required-evidence ["EIN record" "SAM.gov registration record" "State business registration record" "SAM UEI verification record"]}
   "FRA" {:name "France" :owner-authority "PLACE" :legal-basis "Code de la commande publique" :national-spec "PLACE" :provenance "https://www.marches-publics.gouv.fr/"
          :required-evidence ["SIRET record" "PLACE registration" "RCS extract" "Authorized-representative record"]}
   "TUN" {:name "Tunisia" :owner-authority "TUNEPS" :legal-basis "Public Procurement Decree" :national-spec "TUNEPS" :provenance "https://www.tuneps.tn/"
          :required-evidence ["RNE record" "TUNEPS registration" "Tax ID record" "Authorized-representative record"]}})

(defn spec-basis [iso3] (get catalog iso3))
(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s) missing (remove catalog iso3s)]
     {:requested (count iso3s) :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note "R0 catalog seed"})))
(defn required-evidence-satisfied? [iso3 submitted]
  (when-let [{:keys [required-evidence]} (spec-basis iso3)]
    (= (count required-evidence) (count (filter (set submitted) required-evidence)))))
(defn evidence-checklist [iso3] (:required-evidence (spec-basis iso3) []))
(defn rep-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:rep-owner-authority sb)
      (select-keys sb [:rep-owner-authority :rep-legal-basis :rep-provenance]))))
(defn corporate-number-spec-basis [iso3]
  (when-let [sb (spec-basis iso3)]
    (when (:corporate-number-owner-authority sb)
      (select-keys sb [:corporate-number-owner-authority :corporate-number-legal-basis :corporate-number-provenance]))))
