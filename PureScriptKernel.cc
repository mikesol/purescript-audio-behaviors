/**
 * Copyright 2018 Google LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

#include "emscripten/bind.h"
#include <cstring>
#include "Data_Functor/Data_Functor.h"
#include "FRP_Behavior/FRP_Behavior.h"
#include "FRP_Event/FRP_Event.h"
#include "Data_Array/Data_Array.h"
#include "Data_Semiring/Data_Semiring.h"
#include "Control_Apply/Control_Apply.h"
#include "Data_Semigroup/Data_Semigroup.h"
#include "FRP_Behavior_Audio/FRP_Behavior_Audio.h"
#include "Heterogeneous_Mapping/Heterogeneous_Mapping.h"
#include "Effect_Class_Console/Effect_Class_Console.h"
#include "Effect_Class/Effect_Class.h"
#include "Data_Show/Data_Show.h"
#include "Main/Main.h"
#include "purescript.h"
using namespace emscripten;
using namespace purescript;

const unsigned kRenderQuantumFrames = 128;
const unsigned kBytesPerChannel = kRenderQuantumFrames * sizeof(float);

// The "kernel" is an object that processes a audio stream, which contains
// one or more channels. It is supposed to obtain the frame data from an
// |input|, process and fill an |output| of the AudioWorkletProcessor.
//
//       AudioWorkletProcessor Input(multi-channel, 128-frames)
//                                 |
//                                 V
//                               Kernel
//                                 |
//                                 V
//       AudioWorkletProcessor Output(multi-channel, 128-frames)
//
// In this implementation, the kernel operates based on 128-frames, which is
// the render quantum size of Web Audio API.
static const int sinkLen = kRenderQuantumFrames * (2 << 8);
class PureScriptKernel
{
private:
  float sink[sinkLen] = {};
  unsigned current_sample = 0;

public:
  PureScriptKernel() {}

  void Process(uintptr_t input_ptr, uintptr_t output_ptr,
               unsigned channel_count, unsigned sample_rate)
  {
    const int toAdd = channel_count * kRenderQuantumFrames;
    std::memcpy(sink, sink + toAdd, (sinkLen - toAdd) * sizeof(float));

    float *input_buffer = reinterpret_cast<float *>(input_ptr);
    float *output_buffer = reinterpret_cast<float *>(output_ptr);

    std::memcpy(sink + (sinkLen - toAdd), input_buffer, toAdd * sizeof(float));
    const unsigned chunks = sinkLen / toAdd;

    array_t inner;

    // Bypasses the data. By design, the channel count will always be the same
    // for |input_buffer| and |output_buffer|.

    auto stpos = sinkLen - (chunks * toAdd);
    for (unsigned channel = 0; channel < channel_count; ++channel)
    {
      array_t ch;
      const auto chpos = channel * kRenderQuantumFrames;
      for (unsigned chunk = 0; chunk < chunks; chunk++)
      {
        const auto inChunk = chunk * toAdd;
        auto const st = sink + stpos + inChunk + chpos;
        ch.insert(ch.end(), st, st + kRenderQuantumFrames);
      }
      inner.emplace_back(ch);
    }

    array_t outer;

    outer.emplace_back(inner);
    boxed aud = FRP_Behavior_Audio::audioIO()(undefined)(undefined)(Heterogeneous_Mapping::hmapRecord()(undefined)(Heterogeneous_Mapping::mapRecordWithIndexNil()))(Main::delayProcessor())(static_cast<int>(sample_rate))(static_cast<int>(kRenderQuantumFrames))(dict_t{

    })(static_cast<int>(current_sample))(outer)();
    const auto &inner_ = unbox<array_t>(aud);

    for (unsigned channel = 0; channel < channel_count; ++channel)
    {
      float *destination = output_buffer + channel * kRenderQuantumFrames;
      const auto &channelArray_ = unbox<array_t>(inner_[channel]);
      for (unsigned frame = 0; frame < kRenderQuantumFrames; frame++)
      {
        destination[frame] = unbox<float>(channelArray_[frame]);
      }
    }

    //std::memcpy(output_buffer, sink + (sinkLen - toAdd), toAdd * sizeof(float));
    current_sample = current_sample + kRenderQuantumFrames;
  }
};

EMSCRIPTEN_BINDINGS(CLASS_PureScriptKernel)
{
  class_<PureScriptKernel>("PureScriptKernel")
      .constructor()
      .function("process",
                &PureScriptKernel::Process,
                allow_raw_pointers());
}
