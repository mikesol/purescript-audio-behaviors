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
static const int sinkLen = kRenderQuantumFrames * (2 << 10);
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

    array_t toProcess;
    toProcess.insert(toProcess.end(), sink, sink + sinkLen);

    // Bypasses the data. By design, the channel count will always be the same
    // for |input_buffer| and |output_buffer|.

    boxed aud = FRP_Behavior_Audio::audioIOInterleaved()(undefined)(undefined)(Heterogeneous_Mapping::hmapRecord()(undefined)(Heterogeneous_Mapping::mapRecordWithIndexNil()))(Main::delayProcessor())(static_cast<int>(sample_rate))(dict_t{

    })(static_cast<int>(current_sample))(static_cast<int>(channel_count))(static_cast<int>(kRenderQuantumFrames))(toProcess)();
    for (unsigned int i = 0; i < toAdd; i++)
    {
      output_buffer[i] = unbox<float>(aud[i]);
    }
    // for now, just copy input to output
    // this undoes all the work above, but allows us to measure
    // how slow the system is by listening to the unadulterated sound
    // and seeing if it is too choppy
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
