#' Data from Joshi and Mason (2008) on voter turnout in Nepal
#'
#' Data from a study on the relationship between land tenure and
#' voter turnout in the three rounds of parliamentary elections
#' in Nepal from the restoration of democracy in 1990 to 1999.
#' Data is at the district-level (N = 75). Variable names are
#' taken directly from original dataset. The data is publicly available
#' and has been included here with the endorsement of the authors.
#'
#' @docType data
#'
#' @usage data(nepaldem)
#'
#' @format A data frame with 76 rows and 73 variables:
#' \describe{
#'  \item{sn}{a column of identifiers. This column is not a variable}
#'  \item{district}{names of the district in Nepal used in analysis}
#'  \item{householdsize}{average size of household in district}
#'  \item{total_holding}{total land holding}
#'  \item{noown_single_tenure}{number of households that own and cultivate
#'  land under single tenure}
#'  \item{norent_single_ten}{number of households that rent for service
#'  and cultivate land under single tenure}
#'  \item{noother_single_ten}{number of households that cultivate
#'  under single tenure and have another set up other than those above}
#'  \item{nomore1_ten_hold}{number of households with more than one tenure}
#'  \item{noholding_below1_pa}{number of households that hold less than
#'  1.0 hectares of land}
#'  \item{noholding_2to3_pa}{number of households that hold 2 to 3
#'  hectares of land}
#'  \item{noholding_4to5_pa}{number of households that hold 4 to 5
#'  hectares of land}
#'  \item{noholding_6to9_pa}{number of households that hold 6 to 9
#'  hectares of land}
#'  \item{noholding_10_pa}{number of households with more than 10
#'  parcels of land}
#'  \item{total_ha}{total hectares of land}
#'  \item{total_parcel}{total parcels of land}
#'  \item{no_hold_fixmoney2}{subsection of number of households with fixed
#'  cash rent}
#'  \item{no_hold_fixproduct2}{subsection of households with fixed
#'  product rent}
#'  \item{no_hold_share2}{subsection of households participating
#'  in sharecropping}
#'  \item{no_hold_services2}{subsection of households participating
#'  in sharecropping}
#'  \item{no_hold_mortgage2}{subsection of households with a mortgage}
#'  \item{no_hold_fixmoney1}{subsection of households with fixed
#'  cash rent}
#'  \item{no_hold_fixproduct1}{subsection of households with fixed
#'  product rent}
#'  \item{no_hold_share1}{subsection of households participating
#'  in sharecropping}
#'  \item{no_hold_services1}{subsection of households with rent for
#'  service}
#'  \item{no_hold_mortgage1}{subsection of households with a mortgage}
#'  \item{totalhouseholds}{total number of households}
#'  \item{landless}{number of landless households}
#'  \item{totalvoters1991}{total number of voters in 1991}
#'  \item{totalcastedvote1991}{total number of votes cast in 1991}
#'  \item{totalvalidvote1991}{total number of valid votes in 1991}
#'  \item{constituency1991}{constituency in 1991}
#'  \item{totalcontestants1991}{total number of candidates contesting
#'  elections in 1991}
#'  \item{totalvoters1994}{total number of voters in 1994}
#'  \item{totalcastedvote1994}{total number of votes cast in 1994}
#'  \item{totalvalidvote1994}{total number of valid votes in 1994}
#'  \item{constituency1994}{constituency in 1994}
#'  \item{totalcontestants1994}{total number of candidates contesting
#'  elections in 1994}
#'  \item{togalvoters1999}{total number of voters in 1999}
#'  \item{totalcastedvote1999}{total number of votes cast in 1999}
#'  \item{totalvalidvote1999}{total number of valid votes in 1999}
#'  \item{constituency1999}{constituency in 1999}
#'  \item{totalcontestants1999}{total number of candidates contesting
#'  elections in 1999}
#'  \item{pop_2001}{population in 2001}
#'  \item{hdi_1996}{HDI 1996 (index 0 to 1)}
#'  \item{per_without_instcredit}{percent without access to institutional
#'  credit}
#'  \item{access_instutional_credit}{access to institutional credit}
#'  \item{total_hh_sharecrop}{total number of households participating
#'  in sharecropping}
#'  \item{total_hh_fixmoney}{total number of households with fixed
#'  cash rent}
#'  \item{total_hh_fixproduct}{total number of households with fixed
#'  product rent}
#'  \item{total_hh_service}{total number of households with rent for
#'  service}
#'  \item{total_hh_mortgage}{total number of households with a mortgage}
#'  \item{total_killed}{total number of people killed. This serves as
#'  a measure of political violence during the insurgency}
#'  \item{percent_regvote1991}{election turnout for 1991 as measured
#'  by the percentage of registered voters who voted in the national
#'  parliamentary election}
#'  \item{percent_regvote1994}{election turnout for 1994 as measured
#'  by the percentage of registered voters who voted in the national
#'  parliamentary election}
#'  \item{percent_regvote1999}{election turnout for 1999 as measured
#'  by the percentage of registered voters who voted in the national
#'  parlimentary election}
#'  \item{per_total_hold_sharecrop}{percent of sharecropping households}
#'  \item{per_total_hold_fixmoney}{percent of households that have a
#'  fixed cash rent}
#'  \item{per_total_hold_fixproduct}{percent of households that have a
#'  fixed product rent}
#'  \item{per_total_hold_service}{percent of households that have rent
#'  for service}
#'  \item{per_total_hold_mortgage}{percent of households with a mortgage}
#'  \item{per_noholding_below1_pa}{}
#'  \item{landless_1000}{landless households (in 1,000s)}
#'  \item{totoalkilled_1000}{total number of people killed (in 1,000s). This
#'  serves as a measure of political violence during the insurgency}
#'  \item{cast_eth_fract}{caste and ethnic fractionalization}
#'  \item{languistic_fract}{linguistic fractionalization}
#'  \item{landless_gap}{landless households (in 1,000s) gap}
#'  \item{below1pa_gap}{percent smallholder households gap}
#'  \item{sharecrop_gap}{percent sharecropping households gap}
#'  \item{service_gap}{percent rent for service households gap}
#'  \item{fixmoney_gap}{percent fixed cash rent households gap}
#'  \item{fixprod_gap}{percent fixed product rent households gap}
#'  \item{hdi_gap}{HDI 1996 (index 0 to 1) gap}
#'  \item{ln_pop2001}{population in 2001 (logged)}
#'  \item{hdi_gap1}{HDI 1996 (index 0 to 1) gap (positive values)}
#' }
#'
#' @keywords datasets
#'
#' @references Joshi, M., & Mason, T. D. (2008). Between democracy and
#' revolution: peasant support for insurgency versus democracy in Nepal.
#' Journal of Peace Research, 45(6), 765-782.
#' \doi{10.1177/0022343308096155}
#'
#' @source \href{https://www.prio.org/JPR/Datasets/}{Journal of Peace Research Replication Datasets}
#'
#' @examples
#' \dontshow{.old_wd <- setwd(tempdir())}
#' \donttest{
#' data(nepaldem)
#'
#' library(MASS)
#' library(modeLLtest)
#'
#' # Models from Joshi and Mason (2008)
#' model_1991 <- rlm(percent_regvote1991 ~ landless_gap +
#'    below1pa_gap + sharecrop_gap + service_gap + fixmoney_gap +
#'    fixprod_gap + per_without_instcredit + hdi_gap1 + ln_pop2001 +
#'    totalcontestants1991 + cast_eth_fract, data = nepaldem)
#'
#' model_1994 <- rlm(percent_regvote1994 ~ landless_gap +
#'    below1pa_gap + sharecrop_gap + service_gap + fixmoney_gap +
#'    fixprod_gap +  per_without_instcredit + hdi_gap1 + ln_pop2001 +
#'    totalcontestants1994 + cast_eth_fract, data = nepaldem)
#'
#' model_1999a <- rlm(percent_regvote1999 ~ landless_gap +
#'    below1pa_gap + sharecrop_gap + service_gap + fixmoney_gap +
#'    fixprod_gap + per_without_instcredit + hdi_gap1 + ln_pop2001 +
#'    totalcontestants1999 + cast_eth_fract, data = nepaldem)
#'
#' model_1999b <- rlm(percent_regvote1999 ~ landless_gap +
#'    below1pa_gap + sharecrop_gap + service_gap + fixmoney_gap +
#'    fixprod_gap + per_without_instcredit + totoalkilled_1000 +
#'    hdi_gap1 + ln_pop2001 + totalcontestants1999 + cast_eth_fract,
#'    data = nepaldem)
#'
#' # Comparing OLS to RR fit for model_1999b
#' obj_cvdm_jm <- cvdm(percent_regvote1999 ~ landless_gap +
#'    below1pa_gap + sharecrop_gap + service_gap + fixmoney_gap +
#'    fixprod_gap + per_without_instcredit + totoalkilled_1000 +
#'    hdi_gap1 + ln_pop2001 + totalcontestants1999 + cast_eth_fract,
#'    data = nepaldem, method1 = "OLS", method2 = "RLM-MM")
#'
#' obj_cvdm_jm
#'
#' }
#' \dontshow{setwd(.old_wd)}
"nepaldem"
NULL
