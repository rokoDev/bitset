include(FetchContent)

# Setup directory to which dependencies will be downloaded
cmake_path(APPEND FETCHCONTENT_BASE_DIR "${CMAKE_SOURCE_DIR}/../" "deps_content" "${CMAKE_GENERATOR_NAME_WITHOUT_SPACES}")

# If dependencies are not available then download it and make available
FetchContent_Declare(
	utils
	GIT_REPOSITORY https://github.com/rokoDev/utils.git
	GIT_TAG        43a19e2b68097c841d6d37ff9f75f90393dd33ca
)

FetchContent_Declare(
	boost_leaf
	GIT_REPOSITORY https://github.com/boostorg/leaf.git
	GIT_TAG        f2ec744ce9ca1934193b85f7094c1931177a451c
)

# If necessary it's possible to use utils from different location instead of downloaded from GitHub
# string(TOUPPER utils UP_utils)
# set(FETCHCONTENT_SOURCE_DIR_${UP_utils} ${CMAKE_SOURCE_DIR}/../utils)

FetchContent_MakeAvailable(utils boost_leaf)
target_link_libraries(bitset INTERFACE utils boost_leaf)

# If use IDE add utils target into deps source group
set_target_properties(utils boost_leaf PROPERTIES FOLDER deps)
