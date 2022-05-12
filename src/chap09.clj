(ns chap09
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(def my-promise (promise))
(deliver my-promise (+ 1 2))
;; => #promise[{:status :ready, :val 3} 0x585d34f7]
@my-promise
;; => 3

(def query "Clojure for the Brave and True")

(defn make-query-string
  [s]
  (str "/search?q%3D" (str/replace s #" " "+")))

(make-query-string query)
;; => "/search?q%3DClojure+for+the+Brave+and+True"

(defn- make-google-query-string
  [s]
  (str "https://www.google.com" (make-query-string s)))

(defn- make-bing-query-string
  [s]
  (str "https://www.bing.com" (make-query-string s)))

(make-google-query-string query)
;; => "https://www.google.com/search?q%3DClojure+for+the+Brave+and+True"

(defn search-google
  [s]
  (slurp (make-google-query-string s)))

(defn search-bing
  [s]
  ;; (Thread/sleep 1000)
  (slurp (make-bing-query-string s)))

(defn search-google-and-bing-futures
  "Makes a search on Google and Bing in separate threads.
  Limits the output for readability."
  [s]
  (let [google-result (future (search-google s))
        bing-result (future (search-bing s))]
    (println "Google result: " (subs @google-result 0 100))
    (println "Bing result: " (subs @bing-result 0 100))))

(search-google-and-bing-futures query)

(comment
  (search-google "Clojure for the Brave and True")

  (search-bing query)

  ,)

(def default-search-engines ["https://www.google.com" "https://www.bing.com"])

(defn search-bak
  "ex 01"
  [s]
  (let [result (promise)]
    (do
      (future (deliver result (search-google s)))
      (future (deliver result (search-bing s))))
    @result))

(defn search
  "ex 01 & 02"
  ([s] (search s default-search-engines))
  ([s search-engines]
   (let [result (promise)]
     (doseq [engine search-engines]
       (future (deliver result (slurp (str engine (make-query-string s))))))
     @result)))

(search query)
(re-seq #"https?://[^\"]*" (search query))
(re-seq #"https?://[^\"]*" (search query ["https://www.bing.com"]))
(re-seq #"https?://[^\"]*" (search query ["https://www.google.com"]))
;; => ("http://schema.org/WebPage"
;;     "https://www.google.com/logos/doodles/2022/mazisi-kunenes-92nd-birthday-6753651837109407.2-2x.png"
;;     "https://www.google.com/logos/doodles/2022/mazisi-kunenes-92nd-birthday-6753651837109407.2-2x.png"
;;     "https://www.google.de/imghp?hl=de&tab=wi"
;;     "https://maps.google.de/maps?hl=de&tab=wl"
;;     "https://play.google.com/?hl=de&tab=w8"
;;     "https://www.youtube.com/?gl=DE&tab=w1"
;;     "https://news.google.com/?tab=wn"
;;     "https://mail.google.com/mail/?tab=wm"
;;     "https://drive.google.com/?tab=wo"
;;     "https://www.google.de/intl/de/about/products?tab=wh"
;;     "http://www.google.de/history/optout?hl=de"
;;     "https://accounts.google.com/ServiceLogin?hl=de&passive=true&continue=https://www.google.com/webhp&ec=GAZAAQ"
;;     "https://www.google.com/setprefdomain?prefdom=DE&amp;prev=https://www.google.de/&amp;sig=K_B8nMXBwkm0mLwWFdsVLh2Me5CiU%3D")


(defn get-results
  "ex 03"
  [s search-engines]
  (let [promises (map (fn [engine]
                        (future (slurp (str engine (make-query-string s))))) search-engines)]
    (future (map (fn [promise]
                   (re-seq #"https?://[^\"]*" @promise)) promises))))

(def promise-results (get-results "clojure" ["https://www.google.com" "https://www.bing.com"]))
(pprint/pprint @promise-results)
