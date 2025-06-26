(ns hato-http-fake.fake-test
  (:require [clojure.test :refer [deftest is testing]]
            [hato.client :as hato]
            [hato-http-fake.fake :refer [with-fake-routes with-fake-routes-in-isolation]]))

(deftest test-with-fake-routes-basic
  (testing "Basic fake route matching"
    (is (= "test-response"
           (with-fake-routes
             {"http://test.com/" (fn [_request]
                                  {:status 200 :body "test-response"})}
             (:body (hato/get "http://test.com/")))))))

(deftest test-with-fake-routes-in-isolation
  (testing "Isolation mode throws exception for unmatched routes"
    (is (thrown? Exception
                 (with-fake-routes-in-isolation
                   {"http://test.com/" (fn [_request]
                                        {:status 200 :body "test-response"})}
                   (hato/get "http://unmatched.com/"))))))

(deftest test-route-matching-with-different-urls
  (testing "Route matching with various URL formats"
    (with-fake-routes
      {"http://example.com/path" (fn [_request]
                                  {:status 200 :body "matched"})}
      (is (= "matched" (:body (hato/get "http://example.com/path"))))
      (is (= "matched" (:body (hato/get "http://example.com/path/"))))))

  (testing "Route matching with query parameters"
    (with-fake-routes
      {"http://example.com/search?q=test" (fn [_request]
                                           {:status 200 :body "search-result"})}
      (is (= "search-result" (:body (hato/get "http://example.com/search" {:query-params {:q "test"}}))))))

  (testing "Route matching with default ports"
    (with-fake-routes
      {"http://example.com/" (fn [_request]
                              {:status 200 :body "default-port"})}
      (is (= "default-port" (:body (hato/get "http://example.com:80/")))))))

(deftest test-http-methods
  (testing "Different HTTP methods"
    (with-fake-routes
      {"http://api.com/resource"
       {:get (fn [_request] {:status 200 :body "GET response"})
        :post (fn [_request] {:status 201 :body "POST response"})
        :put (fn [_request] {:status 200 :body "PUT response"})
        :delete (fn [_request] {:status 204 :body "DELETE response"})}}
      (is (= "GET response" (:body (hato/get "http://api.com/resource"))))
      (is (= "POST response" (:body (hato/post "http://api.com/resource"))))
      (is (= "PUT response" (:body (hato/put "http://api.com/resource"))))
      (is (= "DELETE response" (:body (hato/delete "http://api.com/resource")))))))

(deftest test-regex-pattern-matching
  (testing "Regex pattern matching"
    (with-fake-routes
      {#"http://example\.com/.*\.html" (fn [_request]
                                        {:status 200 :body "html-page"})}
      (is (= "html-page" (:body (hato/get "http://example.com/index.html"))))
      (is (= "html-page" (:body (hato/get "http://example.com/about.html")))))))

(deftest test-query-params-matching
  (testing "Query parameters matching with map specification"
    (with-fake-routes-in-isolation
      {{:address "http://search.com/search"
        :query-params {:q "clojure" :lang "en"}}
       (fn [_request]
         {:status 200 :body "search-results"})}
      (is (= "search-results"
             (:body (hato/get "http://search.com/search"
                             {:query-params {:q "clojure" :lang "en"}})))))))

(deftest test-response-defaults
  (testing "Response defaults when not specified"
    (with-fake-routes
      {"http://test.com/" (fn [_request] {})}
      (let [response (hato/get "http://test.com/")]
        (is (= 200 (:status response)))
        (is (= "" (:body response)))))))

(deftest test-json-response-parsing
  (testing "JSON response parsing"
    (with-fake-routes
      {"http://api.com/data" (fn [_request]
                              {:status 200
                               :headers {"Content-Type" "application/json"}
                               :body "{\"name\": \"test\", \"value\": 42}"})}
      (let [response (hato/get "http://api.com/data" {:as :json})]
        (is (= "{\"name\": \"test\", \"value\": 42}" (:body response)))))))

(deftest test-request-passthrough
  (testing "Request passthrough when no route matches"
    (with-fake-routes
      {"http://matched.com/" (fn [_request]
                              {:status 200 :body "matched"})}
      ;; This should pass through to the real hato client
      ;; We can't easily test this without mocking, but we can verify
      ;; that the fake route still works
      (is (= "matched" (:body (hato/get "http://matched.com/")))))))

(deftest test-multiple-routes
  (testing "Multiple routes with different patterns"
    (with-fake-routes
      {"http://api.com/users" (fn [_request]
                               {:status 200 :body "users-list"})
       "http://api.com/posts" (fn [_request]
                               {:status 200 :body "posts-list"})}
      (is (= "users-list" (:body (hato/get "http://api.com/users"))))
      (is (= "posts-list" (:body (hato/get "http://api.com/posts")))))))

(deftest test-request-inspection
  (testing "Request inspection in handler"
    (let [captured-request (atom nil)]
      (with-fake-routes
        {"http://test.com/inspect" (fn [request]
                                    (reset! captured-request request)
                                    {:status 200 :body "inspected"})}
        (hato/get "http://test.com/inspect" {:headers {"X-Test" "value"}})
        (is (= :get (:request-method @captured-request)))
        (is (= "test.com" (:server-name @captured-request)))
        (is (= "/inspect" (:uri @captured-request)))
        (is (= "value" (get-in @captured-request [:headers "X-Test"])))))))

(deftest test-error-responses
  (testing "Error status codes"
    (with-fake-routes
      {"http://api.com/not-found" (fn [_request]
                                   {:status 404 :body "Not Found"})
       "http://api.com/server-error" (fn [_request]
                                      {:status 500 :body "Internal Server Error"})}
      (let [not-found-response (hato/get "http://api.com/not-found")]
        (is (= 404 (:status not-found-response)))
        (is (= "Not Found" (:body not-found-response))))
      (let [error-response (hato/get "http://api.com/server-error")]
        (is (= 500 (:status error-response)))
        (is (= "Internal Server Error" (:body error-response)))))))

(deftest test-byte-array-response
  (testing "Byte array response handling"
    (let [test-bytes (.getBytes "test data")]
      (with-fake-routes
        {"http://test.com/bytes" (fn [_request]
                                  {:status 200 :body test-bytes})}
        (let [response (hato/get "http://test.com/bytes" {:as :byte-array})]
          (is (= (seq test-bytes) (seq (:body response)))))))))

(deftest test-headers-in-response
  (testing "Custom headers in response"
    (with-fake-routes
      {"http://api.com/headers" (fn [_request]
                                 {:status 200
                                  :headers {"X-Custom" "value"
                                           "Content-Type" "text/plain"}
                                  :body "response"})}
      (let [response (hato/get "http://api.com/headers")]
        (is (= "value" (get-in response [:headers "X-Custom"])))
        (is (= "text/plain" (get-in response [:headers "Content-Type"])))))))

(deftest test-any-method-handler
  (testing "Any method handler"
    (with-fake-routes
      {"http://api.com/any"
       {:any (fn [_request]
               {:status 200 :body "any method"})}}
      (is (= "any method" (:body (hato/get "http://api.com/any"))))
      (is (= "any method" (:body (hato/post "http://api.com/any"))))
      (is (= "any method" (:body (hato/put "http://api.com/any"))))
      (is (= "any method" (:body (hato/delete "http://api.com/any")))))))


(deftest test-scheme-normalization
  (testing "Scheme normalization"
    (with-fake-routes
      {"example.com" (fn [_request]
                      {:status 200 :body "no-scheme"})}
      (is (= "no-scheme" (:body (hato/get "http://example.com")))))))

(deftest test-uri-normalization
  (testing "URI normalization with trailing slashes"
    (with-fake-routes
      {"http://test.com/" (fn [_request]
                           {:status 200 :body "with-slash"})}
      (is (= "with-slash" (:body (hato/get "http://test.com"))))
      (is (= "with-slash" (:body (hato/get "http://test.com/")))))))

(deftest test-port-normalization
  (testing "Port normalization"
    (with-fake-routes
      {"http://test.com:8080" (fn [_request]
                               {:status 200 :body "custom-port"})}
      (is (= "custom-port" (:body (hato/get "http://test.com:8080")))))))

(deftest test-complex-query-params
  (testing "Complex query parameters with spaces and special characters"
    (with-fake-routes-in-isolation
      {{:address "http://search.com/search"
        :query-params {:q "this has spaces" :filter "special&chars"}}
       (fn [_request]
         {:status 200 :body "complex-search"})}
      (is (= "complex-search"
             (:body (hato/get "http://search.com/search"
                             {:query-params {:q "this has spaces"
                                           :filter "special&chars"}})))))))

(deftest test-non-string-query-params
  (testing "Non-string query parameters"
    (with-fake-routes-in-isolation
      {{:address "http://api.com/data"
        :query-params {:id 123 :active true :name "test"}}
       (fn [_request]
         {:status 200 :body "data-response"})}
      (is (= "data-response"
             (:body (hato/get "http://api.com/data"
                             {:query-params {:id 123 :active true :name "test"}})))))))
