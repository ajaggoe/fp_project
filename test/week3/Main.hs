module Main where

import           Control.Monad
import           Data.List
import           System.Exit
import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

import           Jq.Json                              as Json

instance Arbitrary JSON where
    arbitrary = do
        n <- arbitrary :: Gen Integer
        s <- arbitrary :: Gen String
        b <- arbitrary :: Gen Bool
        xs <- frequency [ (1, return []), (5, do { x <- arbitrary; return [x] })]
        ys <- frequency [ (1, return []), (5, do { x <- arbitrary; s <- arbitrary; return [(s, x)] })]

        elements [ jsonNull, jsonNumber n, jsonString s, jsonBool b, jsonArray xs, jsonObject ys ]

main = defaultMain tests

prop_null_refl      = jsonNull      == jsonNull
prop_number_refl n  = jsonNumber n  == jsonNumber n
prop_string_refl s  = jsonString s  == jsonString s
prop_bool_refl   b  = jsonBool b    == jsonBool b
prop_array_refl  xs = jsonArray xs  == jsonArray xs
prop_object_refl xs = jsonObject xs == jsonObject xs

prop_null_not_equals_number n  = jsonNull /= jsonNumber n
prop_null_not_equals_string s  = jsonNull /= jsonString s
prop_null_not_equals_bool   b  = jsonNull /= jsonBool b
prop_null_not_equals_array  xs = jsonNull /= jsonArray xs
prop_null_not_equals_object xs = jsonNull /= jsonObject xs

prop_number_not_equals_null   n    = jsonNumber n /= jsonNull
prop_number_not_equals_string n s  = jsonNumber n /= jsonString s
prop_number_not_equals_bool   n b  = jsonNumber n /= jsonBool b
prop_number_not_equals_array  n xs = jsonNumber n /= jsonArray xs
prop_number_not_equals_object n xs = jsonNumber n /= jsonObject xs

prop_string_not_equals_null   s    = jsonString s /= jsonNull
prop_string_not_equals_number s n  = jsonString s /= jsonNumber n
prop_string_not_equals_bool   s b  = jsonString s /= jsonBool b
prop_string_not_equals_array  s xs = jsonString s /= jsonArray xs
prop_string_not_equals_object s xs = jsonString s /= jsonObject xs

prop_bool_not_equals_null   b    = jsonBool b /= jsonNull
prop_bool_not_equals_number b n  = jsonBool b /= jsonNumber n
prop_bool_not_equals_string b s  = jsonBool b /= jsonString s
prop_bool_not_equals_array  b xs = jsonBool b /= jsonArray xs
prop_bool_not_equals_object b xs = jsonBool b /= jsonObject xs

prop_array_not_equals_null   xs    = jsonArray xs /= jsonNull
prop_array_not_equals_number xs n  = jsonArray xs /= jsonNumber n
prop_array_not_equals_string xs s  = jsonArray xs /= jsonString s
prop_array_not_equals_bool   xs b  = jsonArray xs /= jsonBool b
prop_array_not_equals_object xs ys = jsonArray xs /= jsonObject ys

prop_object_not_equals_null   xs    = jsonObject xs /= jsonNull
prop_object_not_equals_number xs n  = jsonObject xs /= jsonNumber n
prop_object_not_equals_string xs s  = jsonObject xs /= jsonString s
prop_object_not_equals_bool   xs b  = jsonObject xs /= jsonBool b
prop_object_not_equals_array  xs ys = jsonObject xs /= jsonArray ys

prop_show_null               = show jsonNull == "null"
prop_show_number n           = show (jsonNumber n) == show n
prop_show_string s           = show (jsonString s) == '\"' : s ++ "\""
prop_show_bool_true          = show (jsonBool True) == "true"
prop_show_bool_false         = show (jsonBool False) == "false"
prop_show_empty_array        = show (jsonArray []) == "[]"
prop_show_array_one_element  = show (jsonArray [jsonNull]) == "[\n\tnull\n]"
prop_show_empty_object       = show (jsonObject []) == "{}"
prop_show_object_one_element = show (jsonObject [("key", jsonNull)]) == "{\n\t\"key\":null\n}"

tests = [
    testGroup "Reflection instances" [
        testProperty "Reflection null" prop_null_refl
        testProperty "Reflection number" prop_number_refl
        testProperty "Reflection string" prop_string_refl
        testProperty "Reflection bool" prop_bool_refl
        testProperty "Reflection array" prop_array_refl
        testProperty "Reflection object" prop_object_refl
    ],
    testGroup "Inequality null" [
        testProperty "Inequality null number" prop_null_not_equals_number
        testProperty "Inequality null string" prop_null_not_equals_string
        testProperty "Inequality null bool" prop_null_not_equals_bool
        testProperty "Inequality null array" prop_null_not_equals_array
        testProperty "Inequality null object" prop_null_not_equals_object
    ],
    testGroup "Inequality number" [
        testProperty "Inequality number null" prop_number_not_equals_null
        testProperty "Inequality number string" prop_number_not_equals_string
        testProperty "Inequality number bool" prop_number_not_equals_bool
        testProperty "Inequality number array" prop_number_not_equals_array
        testProperty "Inequality number object" prop_number_not_equals_object
    ],
    testGroup "Inequality string" [
        testProperty "Inequality string null" prop_string_not_equals_null
        testProperty "Inequality string number" prop_string_not_equals_number
        testProperty "Inequality string bool" prop_string_not_equals_bool
        testProperty "Inequality string array" prop_string_not_equals_array
        testProperty "Inequality string object" prop_string_not_equals_object
    ],
    testGroup "Inequality bool" [
        testProperty "Inequality bool null" prop_bool_not_equals_null
        testProperty "Inequality bool number" prop_bool_not_equals_number
        testProperty "Inequality bool string" prop_bool_not_equals_string
        testProperty "Inequality bool array" prop_bool_not_equals_array
        testProperty "Inequality bool object" prop_bool_not_equals_object
    ],
    testGroup "Inequality array" [
        testProperty "Inequality array null" prop_array_not_equals_null
        testProperty "Inequality array number" prop_array_not_equals_number
        testProperty "Inequality array string" prop_array_not_equals_string
        testProperty "Inequality array bool" prop_array_not_equals_bool
        testProperty "Inequality array object" prop_array_not_equals_object
    ],
    testGroup "Inequality object" [
        testProperty "Inequality object null" prop_object_not_equals_null
        testProperty "Inequality object number" prop_object_not_equals_number
        testProperty "Inequality object string" prop_object_not_equals_string
        testProperty "Inequality object bool" prop_object_not_equals_bool
        testProperty "Inequality object array" prop_object_not_equals_array
    ],
    testGroup "Show instances" [
        testProperty "Show null" prop_show_null,
        testProperty "Show number" prop_show_number,
        testProperty "Show string" prop_show_string,
        testProperty "Show true" prop_show_bool_true,
        testProperty "Show false" prop_show_bool_false,
        testProperty "Show empty array" prop_show_empty_array,
        testProperty "Show array one element" prop_show_array_one_element,
        testProperty "Show empty object" prop_show_empty_object,
        testProperty "Show object one element" prop_show_object_one_lement
    ]
]
