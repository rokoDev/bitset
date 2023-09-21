include(FetchContent)

# Setup directory to which dependencies will be downloaded
cmake_path(APPEND FETCHCONTENT_BASE_DIR "${CMAKE_SOURCE_DIR}/../" "deps_content" "${CMAKE_GENERATOR_NAME_WITHOUT_SPACES}")

# If dependencies are not available then download it and make available
FetchContent_Declare(
	utils
	GIT_REPOSITORY https://github.com/rokoDev/utils.git
	GIT_TAG        ff92947f995ac89c9ddc2d16bca83ed089717a2f
)

# If necessary it's possible to use utils from different location instead of downloaded from GitHub
# string(TOUPPER utils UP_utils)
# set(FETCHCONTENT_SOURCE_DIR_${UP_utils} ${CMAKE_SOURCE_DIR}/../utils)

FetchContent_MakeAvailable(utils)
target_link_libraries(bitset INTERFACE utils)

# If use IDE add utils target into deps source group
set_target_properties(utils PROPERTIES FOLDER deps)
