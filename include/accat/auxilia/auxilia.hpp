//===-------- auxilia.hpp - Auxilia Library Header -------*- C++ -*-===//
//
// Part of the auxilia project.
// Licensed under the Apache License v2.0.
//
// auxilia - from Latin word, meaning "help, aid, or assistance".
// A C++23 library for general-purpose utilities.
//
// This header file includes all the components of the auxilia library.
//
//
//   Copyright 2025 LiAuTraver
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.
//
//===---------------------------------------------------------------===//

#pragma once

#include "base/config.hpp"             // IWYU pragma: export
#include "synchronization/id.hpp"      // IWYU pragma: export
#include "memory/memory.hpp"           // IWYU pragma: export
#include "meta/Monostate.hpp"          // IWYU pragma: export
#include "utility/Property.hpp"        // IWYU pragma: export
#include "utility/random.hpp"          // IWYU pragma: export
#include "status/Status.hpp"           // IWYU pragma: export
#include "status/StatusOr.hpp"         // IWYU pragma: export
#include "container/Variant.hpp"       // IWYU pragma: export
#include "utility/bit.hpp"             // IWYU pragma: export
#include "base/format.hpp"             // IWYU pragma: export
#include "ranges/views.hpp"            // IWYU pragma: export
#include "container/Generator.hpp"     // IWYU pragma: export
#include "utility/program_options.hpp" // IWYU pragma: export
#include "container/bitset.hpp"        // IWYU pragma: export
#include "meta/type_traits.hpp"        // IWYU pragma: export
#include "container/chars.hpp"         // IWYU pragma: export
#include "container/Trie.hpp"          // IWYU pragma: export
#include "utility/Noise.hpp"           // IWYU pragma: export
