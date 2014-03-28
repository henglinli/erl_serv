cmake_minimum_required (VERSION 2.8)
project(OneServer)
# deps
set(LibThrift "$ENV{HOME}/usr" CACHE FILEPATH "root path of thrift lib")
# include
include_directories(
${OneServer_SOURCE_DIR}
${LibThrift}/include
/usr/local/include)
# link
#message(STATUS ${PROJECT_BINARY_DIR})
link_directories(
${LibThrift}/lib
/usr/local/lib)
# output
set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR})
# make var
if(UNIX)
  add_definitions()
  if(APPLE)
    set(CMAKE_CXX_COMPILER "clang++")
    #set(CMAKE_CXX_COMPILER "g++-4.9")
    set(CMAKE_CXX_FLAGS "-std=c++11 -Wall -Weffc++")
    #-fno-exceptions
    set(CMAKE_EXE_LINKER_FLAGS "-stdlib=libc++")
    add_definitions()
  else()
    set(CMAKE_CXX_COMPILER "clang++")
    #set(CMAKE_CXX_COMPILER "clang++")
    #set(CMAKE_CXX_FLAGS "-std=c++11 -fno-exceptions -Wall -Wextra -Weffc++")
    set(CMAKE_CXX_FLAGS "-Wall -Wextra")
    set(CMAKE_EXE_LINKER_FLAGS "-pthread")
    add_definitions()
  endif()
endif()
# compile
# gen-cpp
aux_source_directory(thrift/gen-cpp gen-cpp-src)
#add_library(gen-cpp OBJECT ${gen-cpp-src})
add_library(gen-cpp ${gen-cpp-src})
# client
set(client-src client.cpp)
add_executable(client ${client-src})
# link
if(UNIX) 
  if(APPLE)
    # client
    target_link_libraries(client gen-cpp gflags thrift)
  else()
    # client
    target_link_libraries(client gen-cpp gflags thrift)
  endif()
endif()
##
#add_library(gxmpp STATIC $<TARGET_OBJECTS:expat>
#  $<TARGET_OBJECTS:talk_base> 
#  $<TARGET_OBJECTS:talk_xmllite>
#  $<TARGET_OBJECTS:talk_xmpp>
#  $<TARGET_OBJECTS:extra>)
