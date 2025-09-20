## Test environments
* windows 11, R 4.5.1
* github actions check standard (5 OS - all running R 4.5.1)

## R CMD check results (9-29-2025)
Clean on all five OS. 

(9-20-2025)
Remove c++11 requirement. Update testthat file to check for relative
dependency among the methods to avoid instability in the automatic variogram
fitting process.

(2-7-2020)
This update is in response to a request I received from CRAN to remove 
conditionals that use class statements that might create vectors of 
length greater than one. This used to be tolerated, but is no 
longer tolerated on new systems. I also made several updates to the 
documentation. 

(7-3-2019)
As previously mentioned, the requested references can not be provided at this 
time. I will add references to the description once our paper has completed
the review process.

I have confirmed that all examples and tests run using only 1 core. 

(7-2-2019)
Fixed the license template and formatting issues in the description as 
requested. 
The paper outlining the method is currently under review. 
As such, we cannot include a reference with a DOI in the description
in this initial submission. We anticipate adding this reference to 
future iterations of the package.   


