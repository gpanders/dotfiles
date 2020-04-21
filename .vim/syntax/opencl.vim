if exists('b:current_syntax')
    finish
endif

runtime! syntax/c.vim syntax/c_*.vim syntax/c/*.vim

syn keyword clStorage global __global local __local constant __constant private __private
syn keyword clStorage kernel __kernel  __attribute__ read_only __read_only write_only
syn keyword clStorage __write_only read_write __read_write complex imaginary

" Scalar data types
syn keyword clType uchar short ushort uint ulong half quad size_t ptrdiff_t intptr_t uintptr_t

" Vector data types
syn match clType display "[^a-zA-Z0-9_()]\(char\|int\|long\|short\|uchar\|uint\|ulong\|ushort\|double\|float\)\(2\|3\|4\|8\|16\)\=[^a-zA-Z0-9_()]"

" Other data types
syn keyword clType image2d_t image3d_t image2d_array_t image1d_t image1d_buffer_t
syn keyword clType image1d_array_t image2d_depth_t image2d_array_depth_t sampler_t
syn keyword clType queue_t ndrange_t clk_event_t reserve_id_t event_t cl_mem_fence_flags
syn keyword clType image2d_msaa_t image2d_array_msaa_t image2d_msaa_depth_t
syn keyword clType image2d_array_msaa_depth_t cl_image_format

" Abstract data types
syn keyword clType _cl_platform_id _cl_device_id _cl_context _cl_command_queue
syn keyword clType _cl_mem _cl_program _cl_kernel _cl_event _cl_sampler

" Reserved types
syn keyword clType bool2 bool3 bool4 bool8 bool16 half2 half3 half4 half8 half16 quad2
syn keyword clType quad3 quad4 quad8 quad16 float2x2 float2x3 float2x4 float2x8 float2x16
syn keyword clType float3x2 float3x3 float3x4 float3x8 float3x16 float4x2 float4x3 float4x4
syn keyword clType float4x8 float4x16 float8x2 float8x3 float8x4 float8x8 float8x16
syn keyword clType float16x2 float16x3 float16x4 float16x8 float16x16 double2x2 double2x3
syn keyword clType double2x4 double2x8 double2x16 double3x2 double3x3 double3x4 double3x8
syn keyword clType double3x16 double4x2 double4x3 double4x4 double4x8 double4x16 double8x2
syn keyword clType double8x3 double8x4 double8x8 double8x16 double16x2 double16x3
syn keyword clType double16x4 double16x8 double16x16

" Casts
syn keyword clCast vec_type_hint work_group_size_hint aligned packed endian
syn match clCast display "as_\(uchar\|char\|ushort\|short\|uint\|int\|ulong\|long\|float\|double\)"
syn match clCast display "as_\(uchar\|char\|ushort\|short\|uint\|int\|ulong\|long\|float\|double\)\(2\|3\|4\|8\|16\)"
syn match clCast display "convert_\(uchar\|char\|ushort\|short\|uint\|int\|ulong\|long\|float\|double\)\(2\|3\|4\|8\|16\)"
syn match clCast display "convert_\(uchar\|char\|ushort\|short\|uint\|int\|ulong\|long\|float\|double\)\(2\|3\|4\|8\|16\)_sat"
syn match clCast display "convert_\(uchar\|char\|ushort\|short\|uint\|int\|ulong\|long\|float\|double\)\(2\|3\|4\|8\|16\)_sat_\(rte\|rtz\|rtp\|rtn\)"

" Address space qualifier functions.
syn keyword clFunction to_global to_local to_private get_fence

" Async Copy and Prefetch Functions
syn keyword clFunction async_work_group_copy async_work_group__strided_copy wait_group_events prefetch

" Atomic functions
syn keyword clFunction atom_add atom_and atom_cmpxchg atom_dec atom_inc atom_max atom_min
syn keyword clFunction atom_or atom_sub atom_xchg atom_xor atomic_compare_exchange_strong
syn keyword clFunction atomic_compare_exchange_strong_explicit atomic_compare_exchange_weak
syn keyword clFunction atomic_compare_exchange_weak_explicit atomic_exchange
syn keyword clFunction atomic_exchange_explicit atomic_fetch_key atomic_fetch_key_explicit
syn keyword clFunction atomic_flag_clear atomic_flag_clear_explicit atomic_flag_test_and_set
syn keyword clFunction atomic_flag_test_and_set_explicit atomic_init atomic_load
syn keyword clFunction atomic_load_explicit atomic_store atomic_store_explicit
syn keyword clFunction atomic_work_item_fence

" Event functions
syn keyword clFunction capture_event_profiling_info create_user_event is_valid_event
syn keyword clFunction release_event retain_event set_user_event_status

" Maths functions (includes common, geometric, integer)
syn keyword clFunction abs abs_diff acos acosh acospi add_sat asin asinh asinpi atan atan2
syn keyword clFunction atan2pi atanh atanpi cbrt ceil clamp clz copysign cos cosh cospi cross
syn keyword clFunction ctz degrees distance dot erf erfc exp exp10 exp2 expm1 fabs
syn keyword clFunction fast_distance fast_length fast_normalize fdim floor fma fmax fmin fmod
syn keyword clFunction fract frexp hadd half_cos half_divide half_exp half_exp10 half_exp2
syn keyword clFunction half_log half_log10 half_log2 half_powr half_recip half_rsqrt half_sin
syn keyword clFunction half_sqrt half_tan hypot ilogb ldexp length lgamma lgamma_r log log10
syn keyword clFunction log1p log2 logb mad mad24 mad_hi mad_sat max maxmag min minmag mix modf
syn keyword clFunction mul24 mul_hi nan native_cos native_divide native_exp native_exp10
syn keyword clFunction native_exp2 native_log native_log10 native_log2 native_powr native_recip
syn keyword clFunction native_rsqrt native_sin native_sqrt native_tan nextafter normalize
syn keyword clFunction popcount pow pown powr radians remainder remquo rhadd rint rootn rotate
syn keyword clFunction round rsqrt sign sin sincos sinh sinpi smoothstep sqrt step sub_sat tan
syn keyword clFunction tanh tanpi tgamma trunc upsample

" Relational functions
syn keyword clFunction all any bitselect isequal isfinite isgreater isgreaterequal isinf isless
syn keyword clFunction islessequal islessgreater isnan isnormal isnotequal isordered
syn keyword clFunction isunordered select signbit

" Sync functions
syn keyword clFunction barrier mem_fence read_mem_fence sub_group_barrier work_group_barrier
syn keyword clFunction write_mem_fence

" Vector functions (both load/store and misc.)
syn keyword clFunction shuffle shuffle2 vec_step vload2 vload3 vload4 vload8 vload16 vload_half
syn keyword clFunction vload_half2 vload_half3 vload_half4 vload_half8 vload_half16
syn keyword clFunction vloada_half2 vloada_half3 vloada_half4 vloada_half8 vloada_half16
syn keyword clFunction vstore2 vstore3 vstore4 vstore8 vstore16 vstore_half vstore_half2
syn keyword clFunction vstore_half3 vstore_half4 vstore_half8 vstore_half16 vstorea_half2
syn keyword clFunction vstorea_half3 vstorea_half4 vstorea_half8 vstorea_half16

" Work-item functions
syn keyword clFunction get_enqueued_local_size get_enqueued_num_sub_groups get_global_id
syn keyword clFunction get_global_linear_id get_global_offset get_global_size get_group_id
syn keyword clFunction get_local_id get_local_linear_id get_local_size get_max_sub_group_size
syn keyword clFunction get_num_groups get_num_sub_groups get_sub_group_id
syn keyword clFunction get_sub_group_local_id get_sub_group_size get_work_dim

" Pipe functions
syn keyword clFunction commit_read_pipe commit_write_pipe get_pipe_max_packets
syn keyword clFunction get_pipe_num_packets is_valid_reserve_id read_pipe reserve_read_pipe
syn keyword clFunction reserve_write_pipe sub_group_commit_read_pipe
syn keyword clFunction sub_group_commit_write_pipe sub_group_reserve_read_pipe
syn keyword clFunction sub_group_reserve_write_pipe work_group_commit_read_pipe
syn keyword clFunction work_group_commit_write_pipe work_group_reserve_read_pipe
syn keyword clFunction work_group_reserve_write_pipe write_pipe

" image functions
syn keyword clFunction get_image_channel_data_type get_image_channel_order get_image_depth
syn keyword clFunction get_image_dim get_image_height get_image_width

syn match clFunction display "read_image\(f\|i\|ui\|h\)"
syn match clFunction display "write_image\(f\|i\|ui\|h\)"

" Constants
syn keyword clConstant CHAR_BIT CHAR_MAX CHAR_MIN CLK_ADDRESS_CLAMP CLK_ADDRESS_CLAMP_TO_EDGE
syn keyword clConstant CLK_ADDRESS_NONE CLK_ADDRESS_REPEAT CLK_FILTER_LINEAR CLK_FILTER_NEAREST
syn keyword clConstant CLK_GLOBAL_MEM_FENCE CLK_LOCAL_MEM_FENCE CLK_NORMALIZED_COORDS_FALSE
syn keyword clConstant CLK_NORMALIZED_COORDS_TRUE CL_A CL_ARGB CL_BGRA CL_FLOAT CL_HALF_FLOAT
syn keyword clConstant CL_INTENSITY CL_LUMINANCE CL_R CL_RA CL_RG CL_RGB CL_RGBA CL_RGBx CL_RGx
syn keyword clConstant CL_Rx CL_SIGNED_INT16 CL_SIGNED_INT32 CL_SIGNED_INT8 CL_SNORM_INT16
syn keyword clConstant CL_SNORM_INT8 CL_UNORM_INT16 CL_UNORM_INT8 CL_UNORM_INT_101010
syn keyword clConstant CL_UNORM_SHORT_555 CL_UNORM_SHORT_565 CL_UNSIGNED_INT16
syn keyword clConstant CL_UNSIGNED_INT32 CL_UNSIGNED_INT8 DBL_DIG DBL_EPSILON DBL_MANT_DIG
syn keyword clConstant DBL_MAX DBL_MAX_10_EXP DBL_MIN DBL_MIN_10_EXP DBL_MIN_EXP FLT_DIG
syn keyword clConstant FLT_EPSILON FLT_MANT_DIG FLT_MAX FLT_MAX_10_EXP FLT_MAX_EXP FLT_MIN
syn keyword clConstant FLT_MIN_10_EXP FLT_MIN_EXP FLT_RADIX HUGE_VALF INFINITY INT_MAX INT_MIN
syn keyword clConstant LONG_MAX LONG_MIN MAXFLOAT M_1_PI M_2_PI M_2_SQRTPI M_E M_LN10 M_LN2
syn keyword clConstant M_LOG10E M_LOG2E M_PI M_PI2 M_PI4 M_SQRT1_2 M_SQRT2 NAN SCHAR_MAX
syn keyword clConstant SCHAR_MIN SHRT_MAX SHRT_MIN UCHAR_MAX UCHAR_MIN UINT_MAX ULONG_MAX

" Preprocessor directives and macros
syn keyword clConstant CL_DEVICE_MAX_GLOBAL_VARIABLE_SIZE CL_VERSION_1_0 CL_VERSION_1_1
syn keyword clConstant CL_VERSION_1_2 CL_VERSION_2_0 __ENDIAN_LITTLE__ __FAST_RELAXED_MATH__
syn keyword clConstant __IMAGE_SUPPORT__ __OPENCL_C_VERSION__ __OPENCL_VERSION__

" Numbers
" float
syn match clFloat display "\d\+f"
" float, with dot, optional exponent
syn match clFloat display "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
" float, starting with a dot, optional exponent
syn match clFloat display "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
" float, without dot, with exponent
syn match clFloat display "\d\+e[-+]\=\d\+[fl]\=\>"

hi def link clStorage   StorageClass
hi def link clStructure Structure
hi def link clType      Type
hi def link clNormal    Normal
hi def link clVariable  Identifier
hi def link clConstant  Constant
hi def link clCast      Operator
hi def link clFunction  Function
hi def link clStatement Statement
hi def link clFloat     Float

let b:current_syntax = 'opencl'
