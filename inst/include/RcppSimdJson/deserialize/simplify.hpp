#ifndef RCPPSIMDJSON__DESERIALIZE__SIMPLIFY_HPP
#define RCPPSIMDJSON__DESERIALIZE__SIMPLIFY_HPP


#include "../common.hpp"
#include "Type_Doctor.hpp"
#include "scalar.hpp"
#include "vector.hpp"
#include "matrix.hpp"
#include "dataframe.hpp"


namespace rcppsimdjson {
namespace deserialize {


template <Type_Policy type_policy, utils::Int64_R_Type int64_opt, Simplify_To simplify_to>
inline SEXP
simplify_list(simdjson::ondemand::array array, SEXP empty_array, SEXP empty_object, SEXP single_null) {
    Rcpp::List out(static_cast<R_xlen_t>(array.count_elements()));
    auto i = R_xlen_t(0);
    for (auto element : array) {
        simdjson::ondemand::value val;
        if (element.get(val) == simdjson::SUCCESS) {
            out[i++] = simplify_element<type_policy, int64_opt, simplify_to>(
                val, empty_array, empty_object, single_null);
        }
    }
    return out;
}


template <Type_Policy type_policy, utils::Int64_R_Type int64_opt, Simplify_To simplify_to>
inline SEXP
simplify_vector(simdjson::ondemand::array array, SEXP empty_array, SEXP empty_object, SEXP single_null) {
    if (const auto type_doctor = Type_Doctor<type_policy, int64_opt>(array);
        type_doctor.is_vectorizable()) {
        return type_doctor.is_homogeneous()
                   ? vector::dispatch_typed<int64_opt>(
                         array, type_doctor.common_R_type(), type_doctor.has_null())
                   : vector::dispatch_mixed<int64_opt>(array, type_doctor.common_R_type());
    }
    return simplify_list<type_policy, int64_opt, simplify_to>(
        array, empty_array, empty_object, single_null);
}


template <Type_Policy type_policy, utils::Int64_R_Type int64_opt, Simplify_To simplify_to>
inline SEXP
simplify_matrix(simdjson::ondemand::array array, SEXP empty_array, SEXP empty_object, SEXP single_null) {
    if (const auto matrix = matrix::diagnose<type_policy, int64_opt>(array)) {
        return matrix->is_homogeneous
                   ? matrix::dispatch_typed<int64_opt>(array,
                                                       matrix->common_element_type,
                                                       matrix->common_R_type,
                                                       matrix->has_nulls,
                                                       matrix->n_cols)
                   : matrix::dispatch_mixed<int64_opt>(
                         array, matrix->common_R_type, matrix->n_cols);
    }
    return simplify_vector<type_policy, int64_opt, simplify_to>(
        array, empty_array, empty_object, single_null);
}


template <Type_Policy type_policy, utils::Int64_R_Type int64_opt, Simplify_To simplify_to>
inline SEXP simplify_data_frame(simdjson::ondemand::array array,
                                SEXP                 empty_array,
                                SEXP                 empty_object,
                                SEXP                 single_null) {
    if (const auto cols = diagnose_data_frame<type_policy, int64_opt>(array)) {
        return build_data_frame<type_policy, int64_opt, simplify_to>(
            array, cols->schema, empty_array, empty_object, single_null);
    }
    return simplify_matrix<type_policy, int64_opt, simplify_to>(
        array, empty_array, empty_object, single_null);
}


template <Type_Policy type_policy, utils::Int64_R_Type int64_opt, Simplify_To simplify_to>
inline SEXP dispatch_simplify_array(simdjson::ondemand::array array,
                                    SEXP                 empty_array,
                                    SEXP                 empty_object,
                                    SEXP                 single_null) {
    if (array.is_empty()) {
        return empty_array;
    }

    if constexpr (simplify_to == Simplify_To::data_frame) {
        return simplify_data_frame<type_policy, int64_opt, Simplify_To::data_frame>(
            array, empty_array, empty_object, single_null);
    }

    if constexpr (simplify_to == Simplify_To::matrix) {
        return simplify_matrix<type_policy, int64_opt, Simplify_To::matrix>(
            array, empty_array, empty_object, single_null);
    }

    if constexpr (simplify_to == Simplify_To::vector) {
        return simplify_vector<type_policy, int64_opt, Simplify_To::vector>(
            array, empty_array, empty_object, single_null);
    }

    if constexpr (simplify_to == Simplify_To::list) {
        return simplify_list<type_policy, int64_opt, Simplify_To::list>(
            array, empty_array, empty_object, single_null);
    }
}


template <Type_Policy type_policy, utils::Int64_R_Type int64_opt, Simplify_To simplify_to>
inline SEXP simplify_object(simdjson::ondemand::object object,
                            SEXP                        empty_array,
                            SEXP                        empty_object,
                            SEXP                        single_null) {
    if (object.is_empty()) {
        return empty_object;
    }
    size_t n{0};
    for(auto f : object) { n++; }
    object.reset();

    Rcpp::List            out(n);
    Rcpp::CharacterVector out_names(n);

    auto i = R_xlen_t(0L);
    for (auto field : object) {
        std::string_view key;
        if (field.unescaped_key().get(key) == simdjson::SUCCESS) {
            out_names[i] = Rcpp::String(std::string(key));
            out[i] = simplify_element<type_policy, int64_opt, simplify_to>(
                field.value(), empty_array, empty_object, single_null);
            i++;
        }
    }

    out.attr("names") = out_names;
    return out;
}


/**
 * @brief Simplify a @c simdjson::dom::element to an R object.
 *
 *
 * @tparam type_policy The @c Type_Policy specifying type strictness in combining mixed-type array
 * elements into R vectors.
 *
 * @tparam int64_opt The @c Int64_R_Type specifying how big integers are returned to R.
 *
 * @tparam simplify_to The @c Simplify_To specifying the maximum level of simplification.
 *
 *
 * @param element @c simdjson::dom::element to simplify.
 *
 * @param empty_array R object to return when encountering an empty JSON array.
 *
 * @param empty_object R object to return when encountering an empty JSON object.
 *
 *
 * @return The simplified R object ( @c SEXP ).
 *
 *
 * @note definition: forward declaration in @file inst/include/RcppSimdJson/common.hpp @file.
 */
template <Type_Policy type_policy, utils::Int64_R_Type int64_opt, Simplify_To simplify_to>
inline SEXP simplify_element(simdjson::ondemand::value element,
                             SEXP                   empty_array,
                             SEXP                   empty_object,
                             SEXP                   single_null) {
    switch (element.type()) {
        case simdjson::ondemand::json_type::array:
            return dispatch_simplify_array<type_policy, int64_opt, simplify_to>(
                simdjson::ondemand::array(element), empty_array, empty_object, single_null);

        case simdjson::ondemand::json_type::object:
            return simplify_object<type_policy, int64_opt, simplify_to>(
                simdjson::ondemand::object(element), empty_array, empty_object, single_null);

        case simdjson::ondemand::json_type::number:
            {
                simdjson::ondemand::number num = element.get_number();
                simdjson::ondemand::number_type t = num.get_number_type();
                switch (t) {
                    case simdjson::ondemand::number_type::signed_integer:
                        return utils::resolve_int64<int64_opt>(int64_t(num));
                    case simdjson::ondemand::number_type::unsigned_integer:
                        return Rcpp::wrap(std::to_string(uint64_t(num)));
                    case simdjson::ondemand::number_type::floating_point_number:
                        return Rcpp::wrap(double(num));
                }
            }
            break;

        case simdjson::ondemand::json_type::boolean:
            return Rcpp::wrap(bool(element));

        case simdjson::ondemand::json_type::string:
            return Rcpp::wrap(Rcpp::String(std::string(std::string_view(element))));

        case simdjson::ondemand::json_type::null:
            return single_null;
    }

    return R_NilValue; // # nocov
}

inline SEXP simplify_scalar_document(simdjson::ondemand::document_reference doc,
                             SEXP                   single_null) {
    switch (doc.type()) {
        case simdjson::ondemand::json_type::number:
            {
                simdjson::ondemand::number num = doc.get_number();
                simdjson::ondemand::number_type t = num.get_number_type();
                switch(t) {
                    case simdjson::ondemand::number_type::signed_integer:
                        return Rcpp::wrap(int64_t(num));
                    case simdjson::ondemand::number_type::unsigned_integer:
                        return Rcpp::wrap(std::to_string(uint64_t(num)));
                    case simdjson::ondemand::number_type::floating_point_number:
                        return Rcpp::wrap(double(num));
                }
            }
            break;

        case simdjson::ondemand::json_type::boolean:
            return Rcpp::wrap(bool(doc));

        case simdjson::ondemand::json_type::string:
            return Rcpp::wrap(Rcpp::String(std::string(std::string_view(doc))));

        case simdjson::ondemand::json_type::null:
            return single_null;
    }

    return R_NilValue; // # nocov
}


} // namespace deserialize
} // namespace rcppsimdjson

#endif
