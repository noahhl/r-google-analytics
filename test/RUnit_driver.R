# Copyright 2010 Google Inc. All Rights Reserved.
# Author: mpearmain@ (Mike Pearmain)

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Driver file for running RGoogleAnalytics unit tests.
library(RUnit)

r.google.analytics.test.suite <-
  defineTestSuite("RGoogleAnalytics Unit Testing",
                  dir = "./",
                  testFileRegexp =
                  "[a-zA-Z0-9_]+_unittest\\.R$",
                  testFuncRegexp = "^Test+",
                  rngKind = "Marsaglia-Multicarry",
                  rngNormalKind = "Kinderman-Ramage")

test.result <- runTestSuite(r.google.analytics.test.suite)
printTextProtocol(test.result)
