#' Nepal data on voter turnout
#'
#' Data from a study on the relationship bewteen land tenure and
#' voter turnout in the three rounds of parliamentary elections
#' in Nepal from the restoration of democracy in 1990 to 1999.
#' Data is at the district-level (N = 75).
#'
#' @docType data
#'
#' @usage data(nepaldem)
#'
#' @format A data frame with 76 rows and 73 variables:
#' \describe{
#'  \item{district}{district of Nepal}
#'  \item{householdsize}{average size of household in district}
#'  \item{total_holding}{total land holding}
#'  \item{noown_single_tenure}{}
#'  \item{norent_single_ten}{}
#'  \item{noother_single_ten}{}
#'  \item{nomore1_ten_hold}{}
#'  \item{noholding_below1_pa}{}
#'  \item{noholding_2to3_pa}{}
#'  \item{noholding_4to5_pa}{}
#'  \item{noholding_6to9_pa}{}
#'  \item{noholding_10_pa}{}
#'  \item{total_ha}{}
#'  \item{total_parcel}{}
#'  \item{no_hold_fixmoney2}{}
#'  \item{no_hold_fixproduct2}{}
#'  \item{no_hold_share2}{}
#'  \item{no_hold_fixmoney1}{}
#'  \item{no_hold_fixproduct1}{}
#'  \item{no_hold_share1}{}
#'  \item{no_hold_services1}{}
#'  \item{no_hold_mortgage1}{}
#'  \item{no_hold_services1}{}
#'  \item{no_hold_mortgage1}{}
#'  \item{totalhouseholds}{}
#'  \item{landless}{}
#'  \item{totalvoters1991}{}
#'  \item{totalcastedvote1991}{}
#'  \item{totalvalidvote1991}{}
#'  \item{constituency1991}{}
#'  \item{totalcontestants1991}{}
#'  \item{totalvoters1994}{}
#'  \item{totalcastedvote1994}{}
#'  \item{totalvalidvote1994}{}
#'  \item{constituency1994}{}
#'  \item{totalcontestants1994}{}
#'  \item{togalvoters1999}{} ## is this supposed to be 'total'??
#'  \item{totalcastedvote1999}{}
#'  \item{totalvalidvote1999}{}
#'  \item{constituency1999}{}
#'  \item{totalcontestants1999}{}
#'  \item{pop_2001}{}
#'  \item{hdi_1996}{}
#'  \item{per_without_instcredit}{}
#'  \item{access_instutional_credit}{}
#'  \item{total_hh_sharecrop}{}
#'  \item{total_hh_fixmoney}{}
#'  \item{total_hh_fixproduct}{}
#'  \item{total_hh_service}{}
#'  \item{total_hh_mortgage}{}
#'  \item{total_killed}{}
#'  \item{percent_regvote1991}{}
#'  \item{percent_regvote1994}{}
#'  \item{percent_regvote1999}{}
#'  \item{per_total_hold_sharecrop}{}
#'  \item{per_total_hold_fixmoney}{}
#'  \item{per_total_hold_fixproduct}{}
#'  \item{per_total_hold_service}{}
#'  \item{per_total_hold_mortgage}{}
#'  \item{per_noholding_below1_pa}{}
#'  \item{landless_1000}{}
#'  \item{totoalkilled_1000}{} ## is this supposed to be total too??
#'  \item{cast_eth_fract}{}
#'  \item{languistic_fract}{}
#'  \item{landless_gap}{}
#'  \item{below1pa_gap}{}
#'  \item{sharecrop_gap}{}
#'  \item{service_gap}{}
#'  \item{fixmoney_gap}{}
#'  \item{fixprod_gap}{}
#'  \item{hdi_gap}{}
#'  \item{ln_pop2001}{}
#'  \item{hdi_gap1}{}
#' }
#'
#' @keywords datasets
#'
#' @references Joshi, M., & Mason, T. D. (2008). Between democracy and
#' revolution: peasant support for insurgency versus democracy in Nepal.
#' Journal of Peace Research, 45(6), 765-782.
#' <https://doi.org/10.1177/0022343308096155>
#'
#' @source Journal of Peace Research Replication Datasets,
#' <https://www.prio.org/JPR/Datasets/>
#'
#' @examples
#' data(nepaldem)
#' times <- attr(nepaldem, "time")
#' phe <- nepaldem$pheno
#' \donttest{
#' iplotCurves(phe, times, phe[,c(61,121)], phe[,c(121,181)],
#'             chartOpts=list(curves_xlab="Time (hours)", curves_ylab="Root tip angle (degrees)",
#'                            scat1_xlab="Angle at 2 hrs", scat1_ylab="Angle at 4 hrs",
#'                            scat2_xlab="Angle at 4 hrs", scat2_ylab="Angle at 6 hrs"))}
"nepaldem"
