/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <iostream>
//
#include <gflags/gflags.h>
//
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>
//
#include "thrift/gen-cpp/Serv.h"
//
using namespace apache;
//
namespace lee {
// port
DEFINE_int32(port, 9999, "port to connect");
static bool ValidatePort(const char* flag, google::int32 value) {
  if (value > 1024 && value < 32768) {
    return true;
  } else {
    std::cout << "Invalid port for " << flag << " should (1024, 32768)\n";
    return false;
  }
}
// host
DEFINE_string(host, "localhost", "host to connect");
// usage
const std::string usage_string = "Usage: server --host=localhost --port=9999";
// version
const std::string version_string = "1";
} // namespace lee
//
int main(int argc, char** argv) {
  //
  google::SetUsageMessage(lee::usage_string);
  google::SetVersionString(lee::version_string);
  google::RegisterFlagValidator(&lee::FLAGS_port, &lee::ValidatePort);
  google::ParseCommandLineFlags(&argc, &argv, true);
  //
  std::string host = lee::FLAGS_host;
  int port = lee::FLAGS_port;
  // socket
  boost::shared_ptr<thrift::transport::TSocket>
    socket(new thrift::transport::TSocket(host, port));
  // transport
  boost::shared_ptr<thrift::transport::TTransport> 
    transport(new thrift::transport::TFramedTransport(socket));
  // protocol
  boost::shared_ptr<thrift::protocol::TProtocol>
    protocol(new thrift::protocol::TBinaryProtocol(transport));
  //
  user::ServClient client(protocol);
  //
  try {
    std::cout << "open ...\n";
    transport->open();

    //ping
    std::cout << "Ping ...\n";
    client.Ping();

    // get version
    std::cout << "GetVerion ...\n";
    std::string version;
    client.GetVersion(version);
    std::cout << "version:" << version << std::endl;
    //
    /*
    captcha::Captcha captcha;
    //
    user::User user;
    account::Account account;
    account.name = "leelee";
    account.token = "leelee";
    user.account = account;
    //
    try {
      // register
      client.Register(captcha, user);
      std::cout << "captcha image:" << captcha.image << std::endl;
      std::cout << "captcha code:" << captcha.code << std::endl;
    } catch (user::Error& e) {
      std::cout << e.type << ":" << e.message << std::endl;
    }
    //
    try {
      // veryfy
      client.VeryfyRegister(user, "captcha");
      std::cout << "register done\n";
    } catch (user::Error& e) {
      std::cout << e.type << ":" << e.message << std::endl;
    }
    //
    try {
    // login
      client.Login(account);
      std::cout << "login done\n";
    } catch (user::Error& e) {
      std::cout << e.type << ":" << e.message << std::endl;
    }
    */
    // getchar();
    transport->close();
  } catch (thrift::TException &tx) {
    std::cout << "ERROR: " << tx.what() << std::endl;
  }
}
