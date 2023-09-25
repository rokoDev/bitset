#include <bitset/bitset.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <utils/utils.h>

#include <bitset>
#include <variant>

#ifdef BOOST_LEAF_NO_EXCEPTIONS

#include <iostream>

namespace boost
{
[[noreturn]] void throw_exception(std::exception const &e)
{
    std::cerr
        << "Terminating due to a C++ exception under BOOST_LEAF_NO_EXCEPTIONS: "
        << e.what();
    std::terminate();
}

struct source_location;
[[noreturn]] void throw_exception(std::exception const &e,
                                  boost::source_location const &)
{
    throw_exception(e);
}
}  // namespace boost

#endif

namespace
{
template <std::size_t N>
using bitset = utils::bitset<N>;

using namespace std::string_view_literals;

class BitsetTestMultiData : public ::testing::Test
{
   protected:
    static constexpr auto kMaskStrings = utils::make_array(
        "0111001101"sv, "11100111001101"sv, "1111000011001100"sv, "0"sv, "1"sv,
        "11111111"sv, "00000000"sv, "010101010"sv, "11110000110011001"sv,
        "01011"sv);
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

TEST(BitsetTests, Test)
{
    std::variant<utils::eBitsetError> error;
    std::size_t invalid_pos{};
    std::size_t errors_count{};
    auto handle_errors = std::make_tuple(
        [&error, &invalid_pos, &errors_count](
            boost::leaf::match<utils::eBitsetError,
                               utils::eBitsetError::out_of_range>,
            std::size_t, std::size_t aPos, const utils::e_source_location &)
        {
            error = utils::eBitsetError::out_of_range;
            invalid_pos = aPos;
            ++errors_count;
            return false;
        },
        [&errors_count](boost::leaf::diagnostic_info const &aUnmatched)
        {
            std::cerr << "Unknown failure detected\n"
                      << "Cryptic diagnostic information follows\n"
                      << aUnmatched;
            ++errors_count;
            return false;
        });

    bitset<10> b{"0000000000"sv};
    ASSERT_TRUE(b.test(0).has_value());
    ASSERT_FALSE(b.test(b.size()).has_value());
    boost::leaf::try_handle_all(
        [&b]() -> utils::result<bool>
        {
            [[maybe_unused]] bool bit{};
            for (std::size_t i = 0; i <= b.size(); ++i)
            {
                BOOST_LEAF_ASSIGN(bit, b.test(i));
            }
            return {};
        },
        handle_errors);

    ASSERT_TRUE(std::holds_alternative<utils::eBitsetError>(error));
    ASSERT_EQ(std::get<utils::eBitsetError>(error),
              utils::eBitsetError::out_of_range);
    ASSERT_EQ(invalid_pos, b.size());
    ASSERT_EQ(errors_count, 1);
}
