(ns grade-management-system.core (:gen-class))
(defstruct student :name :score :sum :average :rank)

(defn scan []
  (vector (read-line)
          (read-string (read-line))
          (read-string (read-line))
          (read-string (read-line))))

(defn create [haystack]
  (println (apply str (repeat 51 "=")))
  (map #(struct student (first %) (rest %)) haystack))

(defn flip [haystack]
  (vec (apply map vector (vec haystack))))

(defn reckon [haystack]
  (map-indexed #(struct student nil %2) (flip (map :score haystack))))

(defn figure [haystack]
  (map #(let [scr (% :score) sum (apply + scr)
              avg (double (/ sum (count scr)))]
          (assoc % :sum sum :average avg)) haystack))

(defn arrange [haystack]
  (map #(assoc % :rank (keep-indexed
                         (fn [i v] (if (= % v) (inc i)))
                         (reverse (sort-by :sum haystack)))) haystack))

(defn display [haystack]
  (map #(println (apply format
                        "%s\t%d\t%d\t%d\t%d\t%.1f\t%d"
                        (flatten (vals %)))) haystack))

(defn summary [haystack]
  (map-indexed #(println (apply format
                                (get ["합계\t%d\t%d\t%d\t%d" "평균\t%.1f\t%.1f\t%.1f\t%.1f"] %1)
                                (conj %2 (reduce + %2)))) (flip (map #((juxt :sum :average) %) haystack))))

(defn exec [& students]
  (println "Name\t국어\t영어\t수학\t합계\t평균\t석차")
  (doall (->> students (create) (figure) (arrange) (display)))
  (doall (->> students (create) (reckon) (figure) (summary))))

(defn -main [& args]
  (exec (scan) (scan) (scan) (scan) (scan)))
