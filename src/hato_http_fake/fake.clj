(ns hato-http-fake.fake
  (:import [java.util.regex Pattern]
           [java.util Map])
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :refer [cartesian-product]]
            [robert.hooke :refer [add-hook]]
            [ring.util.codec :as ring-codec]
            [hato.client :as hato]))

(def ^:dynamic *fake-routes* {})
(def ^:dynamic *in-isolation* false)

(defmacro with-fake-routes-in-isolation
  "Makes all wrapped hato requests first match against given routes.
   If no route matches, an exception is thrown."
  [routes & body]
  `(binding [*in-isolation* true]
     (with-fake-routes ~routes ~@body)))

(defmacro with-fake-routes
  "Makes all wrapped hato requests first match against given routes.
   The actual HTTP request will be sent only if no matches are found."
  [routes & body]
  `(let [s# ~routes]
     (assert (map? s#))
     (binding [*fake-routes* s#]
       ~@body)))

(defmacro with-global-fake-routes-in-isolation
  [routes & body]
  `(with-redefs [*in-isolation* true]
     (with-global-fake-routes ~routes ~@body)))

(defmacro with-global-fake-routes
  [routes & body]
  `(let [s# ~routes]
     (assert (map? s#))
     (with-redefs [*fake-routes* s#]
       ~@body)))

(defn- defaults-or-value [defaults value]
  (if (contains? defaults value) (reverse (vec defaults)) (vector value)))

(defn- potential-server-ports-for [request-map]
  (defaults-or-value #{80 nil} (:server-port request-map)))

(defn- potential-uris-for [request-map]
  (let [uri (:uri request-map)
        alternatives (cond
                      (nil? uri) #{"/" "" nil}
                      (= uri "/") #{"/" "" nil}
                      (string/ends-with? uri "/")
                      #{uri (string/replace uri #"/$" "")}
                      :else
                      #{uri (str uri "/")})]
    (defaults-or-value alternatives uri)))

(defn- potential-schemes-for [request-map]
  (defaults-or-value #{:http nil} (keyword (:scheme request-map))))

(defn- potential-query-strings-for [request-map]
  (let [queries (defaults-or-value #{"" nil} (:query-string request-map))
        query-supplied (= (count queries) 1)]
    (if query-supplied
      (map (partial string/join "&") (cartesian-product (string/split (first queries) #"&|;")))
      queries)))

(defn- potential-alternatives-to [request]
  (let [schemes (potential-schemes-for request)
        server-ports (potential-server-ports-for request)
        uris (potential-uris-for request)
        query-strings (potential-query-strings-for request)
        ;; cartesian-product will modulate right-most params before left-most params.
        ;; By putting larger collections near the left, we have a higher likelihood
        ;; of taking advantage of its laziness, and halting early
        combinations (cartesian-product query-strings schemes server-ports uris)]
    (map #(merge request (zipmap [:query-string :scheme :server-port :uri] %)) combinations)))

(defn- address-string-for [request-map]
  (let [{:keys [scheme server-name server-port uri query-string]} request-map]
    (string/join [(if (nil? scheme) "" (format "%s://" (name scheme)))
                  server-name
                  (if (nil? server-port) "" (format ":%s" server-port))
                  (if (nil? uri) "" uri)
                  (if (nil? query-string) "" (format "?%s" query-string))])))

(defprotocol RouteMatcher
  (matches [address method request]))

(defn- query-params-match?
  [expected-query-params request]
  (let [actual-query-params (or (some-> request :query-string ring-codec/form-decode) {})]
    (and (= (count expected-query-params) (count actual-query-params))
         (every? (fn [[k v]]
                   (= (str v) (get actual-query-params (name k))))
                 expected-query-params))))

(extend-protocol RouteMatcher
  String
  (matches [address method request]
    (matches (re-pattern (Pattern/quote address)) method request))

  Pattern
  (matches [address method request]
    (let [request-method (:request-method request)
          address-strings (map address-string-for (potential-alternatives-to request))]
      (and (contains? (set (distinct [:any request-method])) method)
           (some #(re-matches address %) address-strings))))

  Map
  (matches [address method request]
    (let [{expected-query-params :query-params} address]
      (and (or (nil? expected-query-params)
               (query-params-match? expected-query-params request))
           (let [request (cond-> request expected-query-params (dissoc :query-string))]
             (matches (:address address) method request))))))

(defn- flatten-routes [routes]
  (let [normalised-routes
        (reduce
         (fn [accumulator [address handlers]]
           (if (map? handlers)
             (into accumulator (map (fn [[method handler]] [method address handler]) handlers))
             (into accumulator [[:any address handlers]])))
         []
         routes)]
    (map #(zipmap [:method :address :handler] %) normalised-routes)))

(defn- build-url-with-query-params [url query-params]
  (if (and query-params (not-empty query-params))
    (let [base-url (if (string/includes? url "?") url (str url "?"))
          query-string (ring-codec/form-encode query-params)]
      (str base-url query-string))
    url))

(defn- parse-hato-request "Parse hato request parameters into a format compatible with our route matching"
  [url options method]
  (let [;; If url is a map (as in hato/request), extract :url
        [url options] (if (map? url)
                         [(:url url) url]
                         [url options])
        query-params (:query-params options)
        full-url (build-url-with-query-params url query-params)
        url-obj (java.net.URL. full-url)
        scheme (.getProtocol url-obj)
        server-name (.getHost url-obj)
        server-port (when (not= -1 (.getPort url-obj)) (.getPort url-obj))
        uri (.getPath url-obj)
        query-string (.getQuery url-obj)
        method (or method :get)]
    {:scheme (keyword scheme)
     :server-name server-name
     :server-port server-port
     :uri uri
     :query-string query-string
     :request-method (keyword (string/lower-case (name method)))
     :body (:body options)
     :headers (:headers options)}))

(defn- create-hato-response
  "Convert our fake response format to hato response format. If the body is a string and Content-Type is JSON, parse it to a map."
  [fake-response]
  (let [body (:body fake-response)
        headers (:headers fake-response {})
        status (:status fake-response 200)
        content-type (get headers "Content-Type")]
    {:status status
     :headers headers
     :body (cond
             (and (string? body)
                  (some? content-type)
                  (re-find #"(?i)application/json" content-type))
             (try
               (require 'cheshire.core)
               ((resolve 'cheshire.core/parse-string) body true)
               (catch Exception _ body))
             :else body)}))

(defn- get-matching-route [request]
  (->> *fake-routes*
       flatten-routes
       (filter #(matches (:address %) (:method %) request))
       first))

(defn- handle-request-for-route [request route]
  (let [route-handler (:handler route)
        response (merge {:status 200 :body ""}
                        (route-handler request))]
    (create-hato-response response)))

(defn- throw-no-fake-route-exception [request]
  (throw (Exception.
          ^String
          (apply format
                 "No matching fake route found to handle request. Request details: \n\t%s \n\t%s \n\t%s \n\t%s \n\t%s "
                 (select-keys request [:scheme :request-method :server-name :uri :query-string])))))

(defn- try-intercept [origfn url options method]
  (let [request (parse-hato-request url options method)]
    (if-let [matching-route (get-matching-route request)]
      (handle-request-for-route request matching-route)
      (if *in-isolation*
        (throw-no-fake-route-exception request)
        (origfn url options)))))

;; Create specific interceptors for each HTTP method
(defn- try-intercept-get
  ([origfn url] (try-intercept-get origfn url {}))
  ([origfn url options] (try-intercept origfn url options :get)))

(defn- try-intercept-post
  ([origfn url] (try-intercept-post origfn url {}))
  ([origfn url options] (try-intercept origfn url options :post)))

(defn- try-intercept-put
  ([origfn url] (try-intercept-put origfn url {}))
  ([origfn url options] (try-intercept origfn url options :put)))

(defn- try-intercept-delete
  ([origfn url] (try-intercept-delete origfn url {}))
  ([origfn url options] (try-intercept origfn url options :delete)))

(defn- try-intercept-head
  ([origfn url] (try-intercept-head origfn url {}))
  ([origfn url options] (try-intercept origfn url options :head)))

(defn- try-intercept-options
  ([origfn url] (try-intercept-options origfn url {}))
  ([origfn url options] (try-intercept origfn url options :options)))

(defn- try-intercept-patch
  ([origfn url] (try-intercept-patch origfn url {}))
  ([origfn url options] (try-intercept origfn url options :patch)))

;; Hook into hato's main request functions
(defn initialize-request-hooks []
  ;; Hook into hato's request function
  (add-hook #'hato/request #'try-intercept)

  ;; Hook into specific HTTP method functions
  (add-hook #'hato/get #'try-intercept-get)
  (add-hook #'hato/post #'try-intercept-post)
  (add-hook #'hato/put #'try-intercept-put)
  (add-hook #'hato/delete #'try-intercept-delete)
  (add-hook #'hato/head #'try-intercept-head)
  (add-hook #'hato/options #'try-intercept-options)
  (add-hook #'hato/patch #'try-intercept-patch))

;; Initialize the hooks when the namespace is loaded
(initialize-request-hooks)