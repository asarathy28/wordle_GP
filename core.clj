(ns evol.core)
(require '[clojure.string :as str])
"Wordle

tiger --> [t i g e r]

guess
[f e q t r]

reward
[0 1 0 1 2] --> 4

"

(def alphabet '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(defn letter-reward [w g wordle]
  (if (= g w)
    2
    (if (some #{g} wordle)
      1
      0)))

(defn reward [genome swordle]
  "Returns the reward of genome."
  (let [wordle (str/split swordle #"")]
    (reduce + (map (fn [x1 x2]
         (letter-reward x1 x2 wordle)) wordle genome))))

#_(reward  '(i h t r e) '(t i g e r))

(defn new-individual [wordle]
  "Returns a new, random individual in the context of test-pairs."
  (let [genome (vec (take (count wordle) (shuffle alphabet)))]
    {:genome genome
     :reward  (reward genome wordle)}))

#_(new-individual "tiger")




(defn best [individuals]
  "Returns the best of the given individuals."
  (reduce (fn [i1 i2]
            (if (> (:reward i1) (:reward i2))
              i1
              i2))
          individuals))
#_(best (repeatedly 100
                    #(new-individual "tiger")))
#_(repeatedly 100
              #(new-individual "tiger"))

(defn select [population]
  "Returns an individual selected from population using a tournament."
  (best (repeatedly 2 #(rand-nth population))))
#_(select (repeatedly 100
                      #(new-individual "tiger")))


;(defn mutate [genome]
;  "Returns a possibly-mutated copy of genome."
;  (let [with-additions (flatten (for [g genome]
;                                  (if (< (rand) 1/10)
;                                    (shuffle (list g (rand-nth ingredients)))
;                                    g)))
;        with-deletions (flatten (for [g with-additions]
;                                  (if (< (rand) 1/11)
;                                    ()
;                                    g)))]
;    (vec with-deletions)))

(defn crossover [genome1 genome2]
  "Returns a one-point crossover product of genome1 and genome2."
  (let [crossover-point (rand-int (inc (min (count genome1)
                                            (count genome2))))]
    (vec (concat (take crossover-point genome1)
                 (drop crossover-point genome2)))))


(defn mutate [genome]
  (if (< (rand) 0.3)
    (assoc genome (rand-int (count genome)) (clojure.string/join "" (take 1 (shuffle alphabet))))
    genome
    ))

(take 1 (shuffle alphabet))
(mutate ["t" "i" "g" "e" "r"])

(clojure.string/join "" (take 1 (shuffle alphabet)))




(defn make-child [population wordle]
  "Returns a new, evaluated child, produced by mutating the result
  of crossing over parents that are selected from the given population."
  (let [new-genome (mutate(crossover (:genome (select population))
                                      (:genome (select population))))]
    {:genome new-genome
     :reward  (reward new-genome wordle)}))
#_(make-child (repeatedly 100
               #(new-individual "tiger"))
              "tiger")

(defn report [generation population]
  "Prints a report on the status of the population at the given generation."
  (let [current-best (best population)]
    (println {:generation   generation
              :best-reward   (:reward current-best)
              :diversity    (float (/ (count (distinct population))
                                      (count population)))
              :average-size (float (/ (->> population
                                           (map :genome)
                                           (map count)
                                           (reduce +))
                                      (count population)))
              :best-genome  (:genome current-best)})))

(defn gp [population-size generations wordle]
  "Runs genetic programming to solve, or approximately solve, a floating-point
  symbolic regression problem in the context of the given population-size,
  number of generations to run, and test-pairs."
  (loop [population (repeatedly population-size
                                #(new-individual wordle))
         generation 0]
    (report generation population)
    (if (or (>= (:reward (best population)) (* 20 (count wordle)))
            (>= generation generations))
      (best population)
      (recur (repeatedly population-size
                         #(make-child population wordle))
             (inc generation)))))

#_(gp 100 100 "thermal")






