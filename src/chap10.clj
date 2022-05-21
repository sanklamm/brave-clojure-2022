(ns chap10
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

;;;;;;;;;;;
;; atoms ;;
;;;;;;;;;;;

(def fred (atom {:cuddle-hunger-level 0
                 :percent-deteriorated 0}))

fred
;; => #atom[{:cuddle-hunger-level 0, :percent-deteriorated 0} 0x16259764]

(deref fred)
;; => {:cuddle-hunger-level 0, :percent-deteriorated 0}

(swap! fred
       (fn [current-state]
         (merge-with + current-state {:cuddle-hunger-level 1})))
;; => {:cuddle-hunger-level 1, :percent-deteriorated 0}

(reset! fred {:foo :bar})
;; => {:foo :bar}

fred
;; => #atom[{:foo :bar} 0x16259764]

(deref fred)
;; => {:foo :bar}

;;;;;;;;;;
;; refs ;;
;;;;;;;;;;

(def sock-varieties
  #{"darned" "argyle" "wool" "horsehair" "mulleted"
    "passive-aggressive" "striped" "polka-dotted"
    "athletic" "business" "power" "invisible" "gollumed"})

(defn sock-count
  [sock-variety count]
  {:variety sock-variety
   :count count})

(defn generate-sock-gnome
  "Create an initial sock gnome state with no socks"
  [name]
  {:name name
   :socks #{}})

(def sock-gnome (ref (generate-sock-gnome "Barumpharumph")))

(def dryer (ref {:name "LG 1337"
                 :socks (set (map #(sock-count % 2) sock-varieties))}))

@dryer
;; => {:name "LG 1337",
;;     :socks
;;     #{{:variety "gollumed", :count 2}
;;       {:variety "striped", :count 2}
;;       {:variety "wool", :count 2}
;;       {:variety "passive-aggressive", :count 2}
;;       {:variety "argyle", :count 2}
;;       {:variety "business", :count 2}
;;       {:variety "darned", :count 2}
;;       {:variety "polka-dotted", :count 2}
;;       {:variety "horsehair", :count 2}
;;       {:variety "power", :count 2}
;;       {:variety "athletic", :count 2}
;;       {:variety "mulleted", :count 2}
;;       {:variety "invisible", :count 2}}}

(defn steal-sock
  [gnome dryer]
  (dosync
   (when-let [pair (some #(if (= (:count %) 2) %) (:socks @dryer))]
     (let [updated-count (sock-count (:variety pair) 1)]
       (alter gnome update-in [:socks] conj updated-count)
       (alter dryer update-in [:socks] disj pair)
       (alter dryer update-in [:socks] conj updated-count)))))

(steal-sock sock-gnome dryer)

(:socks @sock-gnome)
;; => #{{:variety "gollumed", :count 1}}



;;;;;;;;;;
;; vars ;;
;;;;;;;;;;

(def ^:dynamic *notification-address* "dobby@elf.org")

(binding [*notification-address* "test@elf.org"]
  *notification-address*)
;; => "test@elf.org"

*notification-address*
;; => "dobby@elf.org"

(with-redefs [*out* *out*]
  (doto (Thread. #(println "with redefs allows me to show up in the REPL"))
    .start
    .join))


;;;;;;;;;;
;; pmap ;;
;;;;;;;;;;

(defn always-1 [] 1)
(take 5 (repeatedly always-1))

(take 5 (repeatedly (partial rand-int 10)))

(def alphabet-length 26)


;; Vector of chars, A-Z
(def letters (mapv (comp str char (partial + 65)) (range alphabet-length)))

(defn random-string
  "Returns a random string of specified length"
  [length]
  (apply str (take length (repeatedly #(rand-nth letters)))))

(defn random-string-list
  [list-length string-length]
  (doall (take list-length (repeatedly (partial random-string string-length)))))

(def orc-names (random-string-list 3000 7000))

(time (dorun (map clojure.string/lower-case orc-names)))
;; "Elapsed time: 42.18615 msecs"

(time (dorun (pmap clojure.string/lower-case orc-names)))
;; "Elapsed time: 12.423577 msecs"

(def orc-name-abbrevs (random-string-list 20000 300))

(time (dorun (map clojure.string/lower-case orc-name-abbrevs)))
;; "Elapsed time: 15.46159 msecs"

(time (dorun (pmap clojure.string/lower-case orc-name-abbrevs)))
;; "Elapsed time: 35.19287 msecs"

(defn ppmap
  "Partitioned pmap, for grouping map ops together to make parallel  overhead worthwhile"
  [grain-size f & colls]
  (apply concat
         (apply pmap
                (fn [& pgroups] (doall (apply map f pgroups)))
                (map (partial partition-all grain-size) colls))))

(time (dorun (ppmap 1000 clojure.string/lower-case orc-name-abbrevs)))
;; "Elapsed time: 10.166629 msecs"


;;;;;;;;;;
;; ex01 ;;
;;;;;;;;;;

(let [counter (atom 0)]
  (println @counter)
  (dotimes [_ 5]
    (swap! counter inc))
  (println @counter))

;;;;;;;;;;
;; ex02 ;;
;;;;;;;;;;

;; another quote API
(slurp "http://138.201.159.94:8090/quote")

(slurp "https://thesimpsonsquoteapi.glitch.me/quotes")
;; => "[{\"quote\":\"When I catch you, I'm gonna pull out your eyes and stick 'em down your pants so you can watch me kick the crap outta you, okay? Next I'm gonna use your tongue to paint my boat!\",\"character\":\"Moe Szyslak\",\"image\":\"https://cdn.glitch.com/3c3ffadc-3406-4440-bb95-d40ec8fcde72%2FMoeSzyslak.png?1497567512411\",\"characterDirection\":\"Right\"}]"

(json/read-str
 (slurp "https://thesimpsonsquoteapi.glitch.me/quotes")
 :key-fn keyword)

(defn get-random-quote []
  (->
   (json/read-str
    (slurp "https://thesimpsonsquoteapi.glitch.me/quotes")
    :key-fn keyword)
   first
   :quote))

(def quote-count (atom {}))

(get-random-quote)

(->
 get-random-quote
 (str/split #" "))

(let [quote (get-random-quote)]
 (-> quote
     (str/split #" ")
     frequencies))

(defn count-words [s]
 (-> s
     (str/split #" ")
     frequencies))

(defn update-atom-with-map [a m]
  (swap! a
       (fn [current-state]
         (merge-with + current-state m))))


(deref quote-count)

(defn quote-word-count
  "Exercise 2"
  [n]
  (let [word-count (atom {})
        tasks (map
               (fn [_]
                 (future (update-atom-with-map word-count (count-words (get-random-quote)))))
               (range n))]
    (dorun (map deref tasks))
    (->>
     @word-count
     (sort-by val >)
     (take 7)
     flatten)))

(quote-word-count 10)


;;;;;;;;;;
;; ex03 ;;
;;;;;;;;;;

(defn player-validator
  [{:keys [health inventory]}]
  (and
   (<= (:current health) (:max health))
   (>= (:healing-potion inventory) 0)))

(def player-1 (ref {:name "Player 1"
                    :health {:max 40 :current 15}
                    :inventory {:healing-potion 0}}
                   :validator player-validator))

(def player-2 (ref {:name "Player 2"
                    :health {:max 45 :current 45}
                    :inventory {:healing-potion 1}}
                   :validator player-validator))

(defn player-heals-player [p1 p2]
  (dosync
   (alter p1 update-in [:inventory :healing-potion] dec)
   (alter p2 assoc-in [:health :current] (get-in @p2 [:health :max]))))

;; calling this twice produces an IllegalStateException
(player-heals-player player-2 player-1)

(do
  (prn @player-1)
  (prn @player-2))
;; {:name "Player 1", :health {:max 40, :current 40}, :inventory {:healing-potion 0}}
;; {:name "Player 2", :health {:max 45, :current 45}, :inventory {:healing-potion 0}}
