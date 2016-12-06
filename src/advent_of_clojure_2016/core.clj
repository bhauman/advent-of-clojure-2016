(ns advent-of-clojure-2016.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def lines (line-seq (io/reader (io/resource "day4"))))


