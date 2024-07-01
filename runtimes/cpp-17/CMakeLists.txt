cmake_minimum_required(VERSION 3.5)
project(cpp_runtime CXX)

include(CheckIncludeFileCXX)

check_include_file_cxx(any HAS_ANY)
check_include_file_cxx(string_view HAS_STRING_VIEW)
check_include_file_cxx(coroutine HAS_COROUTINE)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_executable(${PROJECT_NAME}
        src/main.cc
        src/RuntimeResponse.h
        src/RuntimeRequest.h
        src/RuntimeLogger.h
        src/$ENV{OPEN_RUNTIMES_ENTRYPOINT})

find_package(Drogon CONFIG REQUIRED)
target_link_libraries(${PROJECT_NAME} 
                      PRIVATE Drogon::Drogon)

target_include_directories(${PROJECT_NAME}
                           PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

target_sources(${PROJECT_NAME}
               PRIVATE ${SRC_DIR})

target_link_libraries(${PROJECT_NAME} PUBLIC curl)
