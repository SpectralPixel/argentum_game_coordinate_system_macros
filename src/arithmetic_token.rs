use std::sync::OnceLock;

use bimap::BiHashMap;

struct ArithmeticToken;

impl ArithmeticToken {
    fn token_name_trait_name_conversions() -> &'static BiHashMap<&'static str, &'static str> {
        static BI_HASHMAP: OnceLock<BiHashMap<&str, &str>> = OnceLock::new();
        BI_HASHMAP.get_or_init(|| {
            let mut conversions = BiHashMap::new();
            conversions.insert("And", "BitAnd");
            conversions.insert("AndEq", "BitAndAssign");
            conversions.insert("Caret", "BitXor");
            conversions.insert("CaretEq", "BitXorAssign");
            conversions.insert("Sub", "Sub");
            conversions.insert("SubEq", "SubAssign");
            conversions.insert("Not", "Not");
            conversions.insert("Or", "BitOr");
            conversions.insert("OrEq", "BitOrAssign");
            conversions.insert("Percent", "Rem");
            conversions.insert("PercentEq", "RemAssign");
            conversions.insert("Plus", "Add");
            conversions.insert("PlusEq", "AddAssign");
            conversions.insert("Shl", "Shl");
            conversions.insert("ShlEq", "ShlAssign");
            conversions.insert("Shr", "Shr");
            conversions.insert("ShrEq", "ShrAssign");
            conversions.insert("Slash", "Div");
            conversions.insert("SlashEq", "DivAssign");
            conversions.insert("Star", "Mul");
            conversions.insert("StarEq", "MulAssign");
        })
    }
}