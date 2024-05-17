roxygen2::roxygenise()
devtools::test()
usethis::use_test()
usethis::use_vignette("pbox_vignette", "The pbox vignette")
#devtools::build()
pkgload::load_all()
prova<-devtools::check()
usethis::use_news_md()
commenti<-usethis::use_cran_comments()
#devtools::release()
libary(goodpractice)
goodpractice::gp()



#####TEST ALL
prova<-devtools::check()
pkgload::load_all()
data(SEAex)
dai<-set_pbox(SEAex[,.(Malaysia,Thailand,Vietnam,avgRegion)])
print(dai)

qpbox(dai,marginal = "Vietnam:31 & avgRegion:26")
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",CI=T)
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32")
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32",CI=T)
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32", fixed = T)
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32",CI=T, fixed = T)
qpbox(dai,marginal = "Vietnam:31 & avgRegion:26",conditional = "Malaysia:32",fixed = T)

library(goodpractice)
goodpractice::gp()



cop<-normalCopula(param = 0.5,dim = 4)
distList<-c("RG" , "SN1", "RG","RG" )
allDistrs<-list(list(mu = 31.07, sigma = 0.28),
                list(mu = 34.4, sigma = 0.98, nu = 1.7),
                list(mu = 31.4, sigma = 0.34),
                list(mu = 25.6, sigma = 0.24))

allDistrs<-list(A=list(mu = 31.07, sigma = 0.28),
     B=list(mu = 34.4, sigma = 0.98, nu = 1.7),
     C=list(mu = 31.4, sigma = 0.34),
     D=list(mu = 25.6, sigma = 0.24))
copSEA <- mvdc(cop, distList,
               allDistrs)
pbox::make_pbox(data=SEAex,copula=copSEA)

data(SEAex)
pbx<-set_pbox(SEAex[,.(Malaysia,Thailand)])


vecQuery <- c(31, 34)
perProb(pbx, vecQuery)


data("SEAex")
pbx<-set_pbox(SEAex)
#Get marginal distribution
qpbox(pbx,mj="Malaysia:33")
#Get Joint distribution
qpbox(pbx,mj="Malaysia:33 & Vietnam:34")
#Get Joint distribution
qpbox(pbx,mj="Vietnam:31", co="avgRegion:26")
#Conditional distribution Pr(X <= x, Y <= y) / Pr(Y <= y)
qpbox(pbx,mj="Malaysia:33 & Vietnam:31", co="avgRegion:26")
#Conditional distribution Pr(X <= x, Y <= y) / Pr(Y = y)
qpbox(pbx,"Malaysia:33 & Vietnam:31", "avgRegion:26",fixed=TRUE)
# Joint distribution with values set on their respective mean value
qpbox(pbx,"mean:c(Vietnam,Thailand)",lower.tail=TRUE)
# Joint distribution with values set on their respective median value
qpbox(pbx,"median:c(Vietnam, Thailand)",lower.tail=TRUE)
# Joint distribution with xxxx
qpbox(pbx,"Malaysia:33 & mean:c(Vietnam, Thailand)",lower.tail=TRUE)
# Conditional distribution distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
qpbox(pbx,"Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)")
# Conditional distribution distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
qpbox(pbx,"Malaysia:33 & median:c(Vietnam,Thailand)", "mean:c(avgRegion)",CI=TRUE,iter=100)

###################################################################################
pkgload::load_all()
data("SEAex")
pbx<-set_pbox(SEAex)
pbx





#Get marginal distribution
qpbox(pbx,mj = "Malaysia:33")

#Get Joint distribution
qpbox(pbx,mj = "Malaysia:33 & Vietnam:34")

# Conditional distribution distribution with Pr(X <= x, Y <= y) / Pr(Y = y)
qpbox(pbx,mj = "Malaysia:33 & median:c(Vietnam,Thailand)", co="mean:c(avgRegion)", fixed=TRUE)

# Estimate confidence intervals
qpbox(pbx,mj = "Vietnam:31 & avgRegion:26", co="Malaysia:32",CI=T)

grid_pbox(pbx, mj = c("Vietnam", "Malaysia"))

seq(-3,3,1)
scenario_pbox(pbx,mj = "Vietnam:31 & avgRegion:26", param_list = list(Vietnam="mu"))

############################################################################
# TESTING FOLDER #
usethis::use_version()
pkgload::load_all()
library(usethis)
#usethis::use_test("qpbox.R")



use_test("qpbox")

usethis::use_version()

pbx<-set_pbox(SEAex[,.(Vietnam,Malaysia,Thailand)])
qpbox(pbx,mj="Vietnam:31",lower.tail = FALSE)
pbx@copula@paramMargins[[1]]
pRG(31, mu = 31.44041, sigma = 0.3473844, lower.tail = FALSE)

prova<-devtools::check()
#usethis::use_news_md()
#usethis:::use_github_action_check_standard()
#commenti<-usethis::use_cran_comments()
#devtools::release()


###############################################################################

# Prepare for CRAN ----

# Update dependencies in DESCRIPTION
# install.packages('attachment', repos = 'https://thinkr-open.r-universe.dev')
attachment::att_amend_desc()

# Check package coverage
covr::package_coverage()
covr::report()

# Run tests
devtools::test()
testthat::test_dir("tests/testthat/")

# Run examples
devtools::run_examples()

# autotest::autotest_package(test = TRUE)

# Check package as CRAN using the correct CRAN repo
withr::with_options(list(repos = c(CRAN = "https://cloud.r-project.org/")),
                    {callr::default_repos()
                      rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran")) })
# devtools::check(args = c("--no-manual", "--as-cran"))

# Check content
# install.packages('checkhelper', repos = 'https://thinkr-open.r-universe.dev')
# All functions must have either `@noRd` or an `@export`.
checkhelper::find_missing_tags()

# Check that you let the house clean after the check, examples and tests
# If you used parallel testing, you may need to avoid it for the next check with `Config/testthat/parallel: false` in DESCRIPTION
all_files_remaining <- checkhelper::check_clean_userspace()
all_files_remaining
# If needed, set back parallel testing with `Config/testthat/parallel: true` in DESCRIPTION

# Check spelling - No typo
# usethis::use_spell_check()
spelling::spell_check_package()

# Check URL are correct
# install.packages('urlchecker', repos = 'https://r-lib.r-universe.dev')
urlchecker::url_check()
urlchecker::url_update()

# check on other distributions
# _rhub v2
rhub::rhub_setup() # Commit, push, merge
rhub::rhub_doctor()
rhub::rhub_platforms()
rhub::rhub_check() # launch manually


# _win devel CRAN
devtools::check_win_devel()
# _win release CRAN
devtools::check_win_release()
# _macos CRAN
# Need to follow the URL proposed to see the results
devtools::check_mac_release()

# Check reverse dependencies
# remotes::install_github("r-lib/revdepcheck")
usethis::use_git_ignore("revdep/")
usethis::use_build_ignore("revdep/")

devtools::revdep()
library(revdepcheck)
# In another session because Rstudio interactive change your config:
id <- rstudioapi::terminalExecute("Rscript -e 'revdepcheck::revdep_check(num_workers = 4)'")
rstudioapi::terminalKill(id)
# if [Exit Code] is not 0, there is a problem !
# to see the problem: execute the command in a new terminal manually.

# See outputs now available in revdep/
revdep_details(revdep = "pkg")
revdep_summary()                 # table of results by package
revdep_report()
# Clean up when on CRAN
revdep_reset()

# Update NEWS
# Bump version manually and add list of changes

# Add comments for CRAN
usethis::use_cran_comments(open = rlang::is_interactive())

# Upgrade version number
usethis::use_version(which = c("patch", "minor", "major", "dev")[1])

# Verify you're ready for release, and release
devtools::release()
