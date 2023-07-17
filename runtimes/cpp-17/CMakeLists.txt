cmake_minimum_required(VERSION 3.5)
project(cpp_runtime CXX)

include(CheckIncludeFileCXX)

check_include_file_cxx(any HAS_ANY)
check_include_file_cxx(string_view HAS_STRING_VIEW)
check_include_file_cxx(coroutine HAS_COROUTINE)

if (NOT "${CMAKE_CXX_STANDARD}" STREQUAL "")
    # Do nothing
elseif (HAS_ANY AND HAS_STRING_VIEW AND HAS_COROUTINE)
    set(CMAKE_CXX_STANDARD 20)
elseif (HAS_ANY AND HAS_STRING_VIEW)
    set(CMAKE_CXX_STANDARD 17)
else ()
    set(CMAKE_CXX_STANDARD 14)
endif ()

set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_executable(${PROJECT_NAME}
        src/main.cc
        src/RuntimeResponse.h
        src/RuntimeRequest.h
        src/$ENV{OPEN_RUNTIMES_ENTRYPOINT})

find_package(Drogon CONFIG REQUIRED)
target_link_libraries(${PROJECT_NAME} 
                      PRIVATE Drogon::Drogon)

if (CMAKE_CXX_STANDARD LESS 17)
    # With C++14, use boost to support any, string_view and filesystem
    message(STATUS "use c++14")
    find_package(Boost 1.61.0 REQUIRED)
    target_link_libraries(${PROJECT_NAME} PUBLIC Boost::boost)
elseif (CMAKE_CXX_STANDARD LESS 20)
    message(STATUS "use c++17")
else ()
    message(STATUS "use c++20")
endif ()

target_include_directories(${PROJECT_NAME}
                           PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_sources(${PROJECT_NAME}
               PRIVATE ${SRC_DIR})

target_link_libraries(${PROJECT_NAME} PUBLIC curl)
