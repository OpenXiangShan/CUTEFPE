#include <stdlib.h>
#include <math.h>
#include <cstdint>
typedef struct{
    int32_t is_subnormal; // 是否是非规格化数
    int32_t is_nan;
    int32_t is_pinf;
    int32_t is_ninf;
    int32_t is_pzero;
    int32_t is_nzero;
} DecodeException;

typedef struct{
    int32_t sign;
    int32_t exponent;
    int32_t mantissa;
    DecodeException exception; // 用于存储解码异常信息
} FloatDecode;

int DecodeExceptionInit(DecodeException* exception) {
    if (!exception) return -1;
    exception->is_subnormal = 0;
    exception->is_nan = 0;
    exception->is_pinf = 0;
    exception->is_ninf = 0;
    exception->is_pzero = 0;
    exception->is_nzero = 0;
    return 0;
}

FloatDecode decode(int32_t f, int dtype) {
    // dtype: 0 - FP32/TF32, 1 - FP16, 2 - BF16
    int exp_width;
    int mantissa_width; //编码的尾数位宽，不包括隐含的1位
    switch(dtype) {
        case 0: // FP32
            exp_width = 8;
            mantissa_width = 23;
            break;
        case 1: // FP16
            exp_width = 5;
            mantissa_width = 10;
            break;
        case 2: // BF16
            exp_width = 8;
            mantissa_width = 7;
            break;
    }

    FloatDecode result;
    DecodeExceptionInit(&result.exception); // 初始化异常信息
    int32_t bits = f;

    int32_t exp_mask = (1 << exp_width) - 1;
    int32_t exp_bias = (1 << (exp_width - 1)) - 1; // 偏移量
    int32_t mantissa_mask = (1 << mantissa_width) - 1;
    int32_t implicit_bit = 1 << mantissa_width; // 隐含的1位
    int32_t max_exp = exp_mask - exp_bias; // 最大指数值

    result.sign = (bits >> (exp_width + mantissa_width)) & 0x1;
    int32_t TempExpo = ((bits >> mantissa_width) & exp_mask) - exp_bias;
    int32_t TempMantissa = bits & mantissa_mask;
    if (TempExpo == -exp_bias) {
        if(TempMantissa == 0) {
            result.exponent = -exp_bias; // Zero
            result.mantissa = 0;
            if(result.sign == 1) {
                result.exception.is_nzero = 1; // Negative zero
            } else {
                result.exception.is_pzero = 1; // Positive zero
            }
        }else{
            result.exponent = -exp_bias + 1; // Denormalized exponent is -126
            result.mantissa = TempMantissa; // For denormalized numbers, mantissa is the raw bits
            result.exception.is_subnormal = 1; // If exponent is -127 and mantissa is not zero, it's a subnormal number
        }
    }
    else if(TempExpo == max_exp && TempMantissa == 0) {
        result.exponent = max_exp; // Positive infinity
        result.mantissa = 0;
        if(result.sign == 1) {
            result.exception.is_ninf = 1; // Negative infinity
        } else {
            result.exception.is_pinf = 1; // Positive infinity
        }
    }
    else if(TempExpo == max_exp && TempMantissa != 0) {
        result.exponent = max_exp; // NaN
        result.mantissa = TempMantissa;
        result.exception.is_nan = 1; // Not a number
    }
    else {
        result.exponent = TempExpo;
        result.mantissa = TempMantissa | implicit_bit; // For normalized numbers, add the implicit leading 1
    }

    return result;
}

FloatDecode decode_fp32(int32_t f) {
    return decode(f, 0); // 0 for FP32
}

FloatDecode decode_fp16(int16_t f) {
    int32_t bits = (int32_t)(*(uint16_t*)&f); // Convert int16_t to int32_t for bit manipulation
    return decode(bits, 1); // 1 for FP16
}

FloatDecode decode_bf16(int16_t f) {
    int32_t bits = (int32_t)(*(uint16_t*)&f); // Convert int16_t to int32_t for bit manipulation
    return decode(bits, 2); // 2 for BF16
}

FloatDecode decode_tf32(int32_t f) {
    FloatDecode decoded = decode(f, 0); // 3 for TF32
    decoded.mantissa >>= 13;
    return decoded;
}

int32_t encode_fp32(FloatDecode decoded) {
    int32_t bits = (decoded.sign << 31) | (((decoded.exponent + 127) & 0xFF) << 23) | (decoded.mantissa & 0x7FFFFF);
    return bits;
}

// 计算先导零个数
int count_leading_zeros(int x, int bits) {
    if (x == 0) return bits;
    int count = 0;
    for (int i = bits - 1; i >= 0; i--) {
        if ((x >> i) & 1) break;
        count++;
    }
    return count;
}

// 获取异常码
// 1-正无穷
// 2-负无穷
// 3-NaN
int32_t get_exceptioncode(float x){
    if (isnan(x)) {
        return 3; // NaN
    } else if (isinf(x)) {
        return (x > 0) ? 1 : 2; // Positive infinity or Negative infinity
    } else {
        return 0; // No exception
    }
}
