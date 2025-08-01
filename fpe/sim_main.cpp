#include <verilated.h>
#include <Vtop.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
//mode 0是从头读/写，1是从上次读写位置继续读/写
// #define FM_DEBUG
int32_t get_exceptioncode(float x){
    if (isnan(x)) {
        return 3; // NaN
    } else if (isinf(x)) {
        return (x > 0) ? 1 : 2; // Positive infinity or Negative infinity
    } else {
        return 0; // No exception
    }
}

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

class FileIntArrayManager {
public:
    enum DataType { INT32, INT16, INT8 };

    char filename[256] = {0}; // 公共文件名属性

private:
    FILE* fp_read = nullptr; // 用于跨多次读取时保存文件指针
    DataType dtype;          // 控制读写的数据类型

public:
    // 构造函数：设置文件名、数据类型，并创建空文件
    FileIntArrayManager(const char* fname, DataType t = INT32, int create = 0) : dtype(t) {
        snprintf(filename, sizeof(filename), "%s", fname);
        FILE* fp = NULL;
        if(create){
            FILE* fp = fopen(filename, "w");
        }
        else{
            FILE* fp = fopen(filename, "r");
        }
        
        if (fp) fclose(fp);
    }

    // 写int32数组
    void write_int32_array_to_file(int32_t* arr, int size, int mode) {
        if (filename[0] == '\0' || dtype != INT32) return;
        const char* fmode = (mode == 1) ? "a" : "w";
        FILE* fp = fopen(filename, fmode);
        if (!fp) {
            perror("fopen failed");
            return;
        }
        for (int i = 0; i < size; ++i) {
            fprintf(fp, "%08x", arr[i]);
        }
        fclose(fp);
    }

    // 写int16数组
    void write_int16_array_to_file(int16_t* arr, int size, int mode) {
        if (filename[0] == '\0' || dtype != INT16) return;
        const char* fmode = (mode == 1) ? "a" : "w";
        FILE* fp = fopen(filename, fmode);
        if (!fp) {
            perror("fopen failed");
            return;
        }
        for (int i = 0; i < size; ++i) {
            fprintf(fp, "%04x", (uint16_t)arr[i]);
        }
        fclose(fp);
    }

    // 写int8数组
    void write_int8_array_to_file(int8_t* arr, int size, int mode) {
        if (filename[0] == '\0' || dtype != INT8) return;
        const char* fmode = (mode == 1) ? "a" : "w";
        FILE* fp = fopen(filename, fmode);
        if (!fp) {
            perror("fopen failed");
            return;
        }
        for (int i = 0; i < size; ++i) {
            fprintf(fp, "%02x", (uint8_t)arr[i]);
        }
        fclose(fp);
    }

    // 读int32数组
    int read_int32_array_from_file(int* arr, int max_size, int mode) {
        if (filename[0] == '\0'/* || dtype != INT32*/) return -1;
        if (mode == 0) {
            if (fp_read) fclose(fp_read);
            fp_read = fopen(filename, "r");
        } else if (!fp_read) {
            fp_read = fopen(filename, "r");
        }
        if (!fp_read) {
            perror("fopen failed");
            return -1;
        }
        int count = 0;
        char buf[9];
        buf[8] = '\0';
        while (count < max_size && fread(buf, 1, 8, fp_read) == 8) {
            sscanf(buf, "%x", &arr[count]);
            float temp_float = *((float*)&arr[count]);
            #ifdef FM_DEBUG
                printf("count:%d read fp32: %#.6a\t, float: %e as float:%f\n", count, temp_float, temp_float, temp_float);
            #endif
            count++;
        }
        if (feof(fp_read)) {
            fclose(fp_read);
            fp_read = NULL;
        }
        return count;
    }

    // 读int16数组
    int read_int16_array_from_file(int16_t* arr, int max_size, int mode) {
        int check_print_mode = 1;
        if (filename[0] == '\0' || dtype != INT16) return -1;
        if (mode == 0) {
            if (fp_read) fclose(fp_read);
            fp_read = fopen(filename, "r");
        } else if (!fp_read) {
            fp_read = fopen(filename, "r");
        }
        if (!fp_read) {
            perror("fopen failed");
            return -1;
        }
        int count = 0;
        char buf[5];
        buf[4] = '\0';
        while (count < max_size && fread(buf, 1, 4, fp_read) == 4) {
            unsigned int tmp;
            sscanf(buf, "%x", &tmp);
            arr[count] = (int16_t)tmp;
            if(check_print_mode == 1){
                // __fp16 temp_fp16 = *((__fp16*)&arr[count]);
                // float temp_float = (float)temp_fp16;
                // #ifdef FM_DEBUG
                //     // printf("count:%d read fp16: %04x, float: %f\n", count, arr[count], temp_float);
                //     printf("count:%d read fp16: %#.3a\t, float: %e as float:%f\n", count, temp_float, temp_float, temp_float);
                // #endif
            }
            else if(check_print_mode == 2){
                int32_t temp_int32 = ((int32_t)arr[count]) << 16;
                float temp_float = *((float*)&temp_int32);
                #ifdef FM_DEBUG
                    // printf("count:%d read bf16: %04x, float: %f\n", count, arr[count], temp_float);
                    printf("count:%d read bf16: %#.2a\t, float: %e as float:%f\n", count, temp_float, temp_float, temp_float);
                #endif
            }

            count++;
        }
        if (feof(fp_read)) {
            fclose(fp_read);
            fp_read = NULL;
        }
        return count;
    }

    // 读int8数组
    int read_int8_array_from_file(int8_t* arr, int max_size, int mode) {
        if (filename[0] == '\0' || dtype != INT8) return -1;
        if (mode == 0) {
            if (fp_read) fclose(fp_read);
            fp_read = fopen(filename, "r");
        } else if (!fp_read) {
            fp_read = fopen(filename, "r");
        }
        if (!fp_read) {
            perror("fopen failed");
            return -1;
        }
        int count = 0;
        char buf[3];
        buf[2] = '\0';
        while (count < max_size && fread(buf, 1, 2, fp_read) == 2) {
            unsigned int tmp;
            sscanf(buf, "%x", &tmp);
            arr[count] = (int8_t)tmp;
            count++;
        }
        if (feof(fp_read)) {
            fclose(fp_read);
            fp_read = NULL;
        }
        return count;
    }

    // 析构时关闭文件指针
    ~FileIntArrayManager() {
        if (fp_read) fclose(fp_read);
    }
};

typedef struct{
    FileIntArrayManager* a; // 文件管理器
    FileIntArrayManager* b; // 文件管理器
    FileIntArrayManager* c; // 文件管理器
    FileIntArrayManager* d; // 文件管理器
} DataFile;

DataFile* create_data_file(const char* a_fname, const char* b_fname, const char* c_fname, const char* d_fname, int dtype, int is_create) {
    DataFile* df = (DataFile*)malloc(sizeof(DataFile));
    if (!df) return NULL;
    if((dtype == 0) || (dtype == 4) || (dtype == 5) || (dtype == 6)){
        df->a = new FileIntArrayManager(a_fname, FileIntArrayManager::INT8, is_create);
        df->b = new FileIntArrayManager(b_fname, FileIntArrayManager::INT8, is_create);
    }
    else if((dtype == 1) || (dtype == 2)){
        df->a = new FileIntArrayManager(a_fname, FileIntArrayManager::INT16, is_create);
        df->b = new FileIntArrayManager(b_fname, FileIntArrayManager::INT16, is_create);
    }
    else{
        df->a = new FileIntArrayManager(a_fname, FileIntArrayManager::INT32, is_create);
        df->b = new FileIntArrayManager(b_fname, FileIntArrayManager::INT32, is_create);
    }
    df->c = new FileIntArrayManager(c_fname, FileIntArrayManager::INT32, is_create);
    df->d = new FileIntArrayManager(d_fname, FileIntArrayManager::INT32, is_create);
    return df;
}

int create_all_types(int dtype, DataFile** df){
    if (dtype == 0){
        *df = create_data_file("int8_a.txt", "int8_b.txt", "int8_c.txt", "int8_d.txt", dtype, 0);
    }
    else if (dtype == 1){
        *df = create_data_file("fp16_a.txt", "fp16_b.txt", "fp16_c.txt", "fp16_d.txt", dtype, 0);
        // *df = create_data_file("fp16_atest.txt", "fp16_btest.txt", "fp16_ctest.txt", "fp16_dtest.txt", dtype, 0);
    }
    else if (dtype == 2){
        *df = create_data_file("bf16_a.txt", "bf16_b.txt", "bf16_c.txt", "bf16_d.txt", dtype, 0);
    }
    else if (dtype == 3){
        *df = create_data_file("tf32_a.txt", "tf32_b.txt", "tf32_c.txt", "tf32_d.txt", dtype, 0);
    }
    else if (dtype == 4){
        *df = create_data_file("i8ui8_a.txt", "i8ui8_b.txt", "i8ui8_c.txt", "i8ui8_d.txt", dtype, 0);
    }
    else if (dtype == 5){
        *df = create_data_file("ui8i8_a.txt", "ui8i8_b.txt", "ui8i8_c.txt", "ui8i8_d.txt", dtype, 0);
    }
    else if (dtype == 6){
        *df = create_data_file("ui8ui8_a.txt", "ui8ui8_b.txt", "ui8ui8_c.txt", "ui8ui8_d.txt", dtype, 0);
    }
    else{
        printf("Invalid dtype: %d\n", dtype);
        return -1;
    }
    return 0;
}

int check_sim(DataFile* df, int bitsize, int dtype, Vtop* top, int cycles, int tolerance){
    int result = 0;
    if (!df || !df->a || !df->b || !df->c || !df->d) {
        return 1;
    }
    int exception_error = 0;
    int data_error = 0;

    int size= bitsize / 32;

    int32_t* a = (int32_t*)malloc(size * sizeof(int32_t));
    int32_t* b = (int32_t*)malloc(size * sizeof(int32_t));
    int32_t* c = (int32_t*)malloc(sizeof(int32_t));
    int32_t* d = (int32_t*)malloc(sizeof(int32_t));


    //循环部分
    printf("cycles : %d\n", cycles);
    for(int j = 0; j < cycles; j ++){

        uint32_t *a_vec = top->io_AVector_bits.data();
        uint32_t *b_vec = top->io_BVector_bits.data();

        df->a->read_int32_array_from_file(a, size, 1);
        df->b->read_int32_array_from_file(b, size, 1);
        df->c->read_int32_array_from_file(c, 1, 1);

        top->clock = 1;
        top->reset = 0;
        top->io_AVector_valid = 1;
        top->io_BVector_valid = 1;
        top->io_CAdd_valid = 1;
        top->io_DResult_ready = 1;
        top->io_opcode = dtype;

        // top->io_AVector_bits = *(uint32_t *)a;
        // top->io_BVector_bits = *(uint32_t *)b;


        // top->io_AVector_bits = *(uint64_t *)a;
        // top->io_BVector_bits = *(uint64_t *)b;

        for(int i = 0; i < size; i ++){
            a_vec[i] = ((uint32_t *) a)[i];
            b_vec[i] = ((uint32_t *) b)[i];
        }


        top->io_CAdd_bits = *(uint32_t *)c;
        top->eval();
        top->clock = 0;
        top->eval();

        uint32_t DRes = top->io_DResult_bits;
        if(top->io_DResult_valid == 1){
            df->d->read_int32_array_from_file(d, 1, 1);
            uint32_t golden_result = *(uint32_t *)d;
            float FDRes = *(float *)&DRes;
            float FGolden = *(float *)&golden_result;
            int32_t FDResExcept = get_exceptioncode(FDRes);
            int32_t FGoldenExcept = get_exceptioncode(FGolden);
            if(FDResExcept != FGoldenExcept){
                exception_error ++;
                printf("Cycle %d: Mismatch! Expected: %x, Got: %x\n", j, golden_result, DRes);
                // printf("golden:%a\n", *(float*)&golden_result);
                // printf("result:%a\n", *(float*)&(DRes));
                // printf("FDResExcept: %d\n", FDResExcept);
                // printf("FGoldenExcept: %d\n", FGoldenExcept);
                // int16_t * fp16_a = (int16_t *)a;
                // int16_t * fp16_b = (int16_t *)b;
                // FloatDecode a0 = decode_fp16(fp16_a[0]);
                // FloatDecode a1 = decode_fp16(fp16_a[1]);
                // FloatDecode b0 = decode_fp16(fp16_b[0]);
                // FloatDecode b1 = decode_fp16(fp16_b[1]);
                // FloatDecode cdecode = decode_fp32(c[0]);
                // printf("product0 exp: %x\n", a0.exponent + b0.exponent);
                // printf("product1 exp: %x\n", a1.exponent + b1.exponent);
                // printf("product1 exp: %x\n", a1.exponent);
                // printf("product1 exp: %x\n", b1.exponent);
                // printf("c exp : %x\n", cdecode.exponent);
                // printf("product0 exp: %x\n", a0.exponent + b0.exponent + 255);
                // printf("product1 exp: %x\n", a1.exponent + b1.exponent + 255);
                // printf("c exp : %x\n", cdecode.exponent + 255);
                // printf("product0 exp: %d\n", a0.exponent + b0.exponent);
                // printf("product1 exp: %d\n", a1.exponent + b1.exponent);
                // printf("c exp : %d\n", cdecode.exponent);
                // free(a);
                // free(b);
                // free(c);
                // free(d);
                // return result;
            }
            else if(((int32_t)DRes - (int32_t)golden_result > tolerance) || ((int32_t)DRes - (int32_t)golden_result < -tolerance)){
                data_error ++;
                // int16_t * fp16_a = (int16_t *)a;
                // int16_t * fp16_b = (int16_t *)b;
                // FloatDecode a0 = decode_fp16(fp16_a[0]);
                // FloatDecode a1 = decode_fp16(fp16_a[1]);
                // FloatDecode b0 = decode_fp16(fp16_b[0]);
                // FloatDecode b1 = decode_fp16(fp16_b[1]);
                // FloatDecode cdecode = decode_fp32(c[0]);
                // int32_t product0_sign = (a0.sign ^ b0.sign)? -1 : 1;
                // int32_t product1_sign = (a1.sign ^ b1.sign)? -1 : 1;
                // int32_t c_sign = cdecode.sign ? -1 : 1;
                // printf("%04x", fp16_a[0] & 0xFFFF);
                // printf("%04x\n", fp16_a[1]& 0xFFFF);
                // printf("%04x", fp16_b[0]& 0xFFFF);
                // printf("%04x\n", fp16_b[1]& 0xFFFF);
                // printf("%08x\n", c[0]);
                // printf("%08x\n", d[0]);
                // printf("DRes - golden = %d\n", (int32_t)DRes - (int32_t)golden_result);
                // printf("Cycle %d: Mismatch! Expected: %x, Got: %x\n", j, golden_result, DRes);
                // printf("golden:%a\n", *(float*)&golden_result);
                // printf("result:%a\n", *(float*)&(DRes));

                // printf("golden float:%f\n", *(float*)&golden_result);
                // printf("result float:%f\n", *(float*)&(DRes));
                // printf("a0 sig: %x\n", a0.mantissa);
                // printf("a1 sig: %x\n", a1.mantissa);
                // printf("b0 sig: %x\n", b0.mantissa);
                // printf("b1 sig: %x\n", b1.mantissa);
                // printf("c  sig: %x\n", cdecode.mantissa);
                // printf("product0 sig: %x\n", a0.mantissa * b0.mantissa * product0_sign);
                // printf("product1 sig: %x\n", a1.mantissa * b1.mantissa * product1_sign);
                // printf("c sig : %x\n", cdecode.mantissa * c_sign);
                // printf("product0 exp: %x\n", a0.exponent + b0.exponent);
                // printf("product1 exp: %x\n", a1.exponent + b1.exponent);
                // printf("c exp : %x\n", cdecode.exponent);
                // // printf("product0 exp: %x\n", a0.exponent + b0.exponent + 255);
                // // printf("product1 exp: %x\n", a1.exponent + b1.exponent + 255);
                // // printf("c exp : %x\n", cdecode.exponent + 255);
                // printf("product0 exp: %d\n", a0.exponent + b0.exponent);
                // printf("product1 exp: %d\n", a1.exponent + b1.exponent);
                // printf("c exp : %d\n", cdecode.exponent);

                // int32_t * tf32_a = (int32_t *)a;
                // int32_t * tf32_b = (int32_t *)b;
                // FloatDecode a0 = decode_tf32(tf32_a[0]);
                // FloatDecode a1 = decode_tf32(tf32_a[1]);
                // FloatDecode b0 = decode_tf32(tf32_b[0]);
                // FloatDecode b1 = decode_tf32(tf32_b[1]);
                // FloatDecode cdecode = decode_fp32(c[0]);
                // int32_t product0_sign = (a0.sign ^ b0.sign)? -1 : 1;
                // int32_t product1_sign = (a1.sign ^ b1.sign)? -1 : 1;
                // int32_t c_sign = cdecode.sign ? -1 : 1;
                // printf("%04x", tf32_a[0]);
                // printf("%04x\n", tf32_a[1]);
                // printf("%04x", tf32_b[0]);
                // printf("%04x\n", tf32_b[1]);
                // printf("%08x\n", c[0]);
                // printf("%08x\n", d[0]);
                // printf("DRes - golden = %d\n", (int32_t)DRes - (int32_t)golden_result);
                // printf("Cycle %d: Mismatch! Expected: %x, Got: %x\n", j, golden_result, DRes);
                // printf("golden:%a\n", *(float*)&golden_result);
                // printf("result:%a\n", *(float*)&(DRes));

                // printf("golden float:%f\n", *(float*)&golden_result);
                // printf("result float:%f\n", *(float*)&(DRes));
                // printf("a0 sig: %x\n", a0.mantissa);
                // printf("a1 sig: %x\n", a1.mantissa);
                // printf("b0 sig: %x\n", b0.mantissa);
                // printf("b1 sig: %x\n", b1.mantissa);
                // printf("c  sig: %x\n", cdecode.mantissa);
                // printf("product0 sig: %x\n", a0.mantissa * b0.mantissa * product0_sign);
                // printf("product1 sig: %x\n", a1.mantissa * b1.mantissa * product1_sign);
                // printf("c sig : %x\n", cdecode.mantissa * c_sign);
                // printf("product0 exp: %x\n", a0.exponent + b0.exponent);
                // printf("product1 exp: %x\n", a1.exponent + b1.exponent);
                // printf("c exp : %x\n", cdecode.exponent);
                // // printf("product0 exp: %x\n", a0.exponent + b0.exponent + 255);
                // // printf("product1 exp: %x\n", a1.exponent + b1.exponent + 255);
                // // printf("c exp : %x\n", cdecode.exponent + 255);
                // printf("product0 exp: %d\n", a0.exponent + b0.exponent);
                // printf("product1 exp: %d\n", a1.exponent + b1.exponent);
                // printf("c exp : %d\n", cdecode.exponent);

                // free(a);
                // free(b);
                // free(c);
                // free(d);
                // return result;
            } else {
                result ++;
            }
        }else
            printf("Cycle %d: Result is Invalid\n",j);
    }
    printf("exc : %d\n", exception_error);
    printf("data : %d\n", data_error);
    free(a);
    free(b);
    free(c);
    free(d);
    return result;
}

int main(int argc, char* argv[]){
    int dtype = 3;
    int cycles = 1000000; // 设置模拟周期数
    int bitsize = 128;
    int tolerance = 1;

    if(argc > 1){
        dtype = atoi(argv[1]);
        printf("Using dtype from arg: %d\n", dtype);
    }

    DataFile* df = NULL;
    
    int create = create_all_types(dtype, &df);
    if(df == NULL)
        printf("df NULL\n");
    Vtop* top = new Vtop;
    top->reset = 1; // 设置复位信号
    top->clock = 1;
    top->eval(); // 评估初始状态
    top->clock = 0; // 释放复位信号
    top->eval(); // 评估复位后的状态
    int result = check_sim(df, bitsize, dtype, top, cycles, tolerance);
    printf("correct: %d / %d\n", result, cycles);
    if(result == cycles)
        printf("Pass!!!\n");
    return 0;
}
