#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <utils/utils.h>

#include "bitset/bitset.h"

namespace
{
template <std::size_t N>
using bitset = utils::bitset<N>;

class BitsetTestMultiData : public ::testing::Test
{
   protected:
    static constexpr auto kMaskStrings = utils::make_array(
        std::string_view("0111001101"), std::string_view("11100111001101"),
        std::string_view("1111000011001100"), std::string_view("0"),
        std::string_view("1"), std::string_view("11111111"),
        std::string_view("00000000"), std::string_view("010101010"),
        std::string_view("11110000110011001"), std::string_view("01011"));
    template <typename Callable, std::size_t... I>
    void run_bitsets_tests(Callable &&aFunc, std::index_sequence<I...>) noexcept
    {
        (aFunc(kMaskStrings[I],
               bitset<kMaskStrings[I].size()>(kMaskStrings[I])),
         ...);
    }
};

class BitsetTest : public ::testing::Test
{
   protected:
    static constexpr std::size_t kBitCount = 10;
    bitset<kBitCount> someBitset;
};
}  // namespace

TEST_F(BitsetTest, DefaultConstructorNonConst)
{
    for (std::size_t i = 0; i < kBitCount; ++i)
    {
        ASSERT_FALSE(someBitset[i]);
    }
    ASSERT_EQ(someBitset.count(), 0);
    ASSERT_EQ(someBitset.size(), kBitCount);
    ASSERT_FALSE(someBitset.all());
    ASSERT_FALSE(someBitset.any());
    ASSERT_TRUE(someBitset.none());
    ASSERT_EQ(someBitset.kNumBytes, 2);
}

TEST_F(BitsetTest, SetAndCheckBits)
{
    for (std::size_t setCount = 1; setCount <= kBitCount; ++setCount)
    {
        for (std::size_t i = 0; i < setCount; ++i)
        {
            someBitset[i] = true;
            ASSERT_TRUE(someBitset[i]);
        }

        for (std::size_t i = setCount; i < kBitCount; ++i)
        {
            ASSERT_FALSE(someBitset[i]);
        }
        ASSERT_EQ(someBitset.count(), setCount);
        if (setCount == kBitCount)
        {
            ASSERT_TRUE(someBitset.all());
        }
        else
        {
            ASSERT_FALSE(someBitset.all());
        }
        ASSERT_TRUE(someBitset.any());
        ASSERT_FALSE(someBitset.none());
    }
}

TEST_F(BitsetTest, ResetAndCheckBits)
{
    for (std::size_t i = 0; i < kBitCount; ++i)
    {
        someBitset[i] = true;
    }
    for (std::size_t resetCount = 1; resetCount <= kBitCount; ++resetCount)
    {
        for (std::size_t i = 0; i < resetCount; ++i)
        {
            someBitset[i] = false;
            ASSERT_FALSE(someBitset[i]);
        }

        for (std::size_t i = resetCount; i < kBitCount; ++i)
        {
            ASSERT_TRUE(someBitset[i]);
        }
        ASSERT_EQ(someBitset.count(), kBitCount - resetCount);
        ASSERT_FALSE(someBitset.all());
        if (resetCount == kBitCount)
        {
            ASSERT_FALSE(someBitset.any());
            ASSERT_TRUE(someBitset.none());
        }
        else
        {
            ASSERT_TRUE(someBitset.any());
            ASSERT_FALSE(someBitset.none());
        }
    }
}

TEST(BitsetTests, FromString)
{
    constexpr std::size_t kBitCount = 10;
    constexpr std::string_view bitMaskStr = "0111001101";
    constexpr bitset<kBitCount> someBitset(bitMaskStr);
    ASSERT_EQ(someBitset.size(), kBitCount);
    std::size_t i = 0;
    for (auto it = bitMaskStr.crbegin(); it < bitMaskStr.crend(); ++it, ++i)
    {
        if (*it == '0')
        {
            ASSERT_FALSE(someBitset[i]);
        }
        else if (*it == '1')
        {
            ASSERT_TRUE(someBitset[i]);
        }
        else
        {
            ASSERT_TRUE(false);  // bit mask can contain only '0' or '1'
        }
    }
}

TEST(BitsetTests, DefaultConstructorConst)
{
    constexpr std::size_t kBitCount = 10;
    const bitset<kBitCount> someBitset;
    for (std::size_t i = 0; i < kBitCount; ++i)
    {
        ASSERT_FALSE(someBitset[i]);
    }
    ASSERT_EQ(someBitset.count(), 0);
    ASSERT_EQ(someBitset.size(), kBitCount);
    ASSERT_FALSE(someBitset.all());
    ASSERT_FALSE(someBitset.any());
    ASSERT_TRUE(someBitset.none());
    ASSERT_EQ(someBitset.kNumBytes, 2);
}

TEST_F(BitsetTestMultiData, CreateFromString)
{
    auto bitset_test = [](auto aSv, auto aBitset)
    {
        ASSERT_EQ(aBitset.size(), aSv.size());
        std::size_t i = 0;
        for (auto it = aSv.crbegin(); it < aSv.crend(); ++it, ++i)
        {
            if (*it == '0')
            {
                ASSERT_FALSE(aBitset[i]);
            }
            else if (*it == '1')
            {
                ASSERT_TRUE(aBitset[i]);
            }
            else
            {
                ASSERT_TRUE(false);  // bit mask can contain only '0' or '1'
            }
        }
    };

    using Indices = std::make_index_sequence<kMaskStrings.size()>;
    run_bitsets_tests(bitset_test, Indices{});
}

TEST_F(BitsetTestMultiData, Flip)
{
    auto bitset_test = [](auto aSv, auto aBitset)
    {
        ASSERT_EQ(aBitset.size(), aSv.size());
        std::size_t i = 0;
        auto flippedBitset = ~aBitset;
        ASSERT_EQ(aBitset.size(), flippedBitset.size());
        for (auto it = aSv.crbegin(); it < aSv.crend(); ++it, ++i)
        {
            if (*it == '0')
            {
                ASSERT_FALSE(aBitset[i]);
                ASSERT_TRUE(flippedBitset[i]);
            }
            else if (*it == '1')
            {
                ASSERT_TRUE(aBitset[i]);
                ASSERT_FALSE(flippedBitset[i]);
            }
            else
            {
                ASSERT_TRUE(false);  // bit mask can contain only '0' or '1'
            }
        }
        flippedBitset.flip();
        ASSERT_EQ(aBitset, flippedBitset);
    };

    using Indices = std::make_index_sequence<kMaskStrings.size()>;
    run_bitsets_tests(bitset_test, Indices{});
}
