macro_rules! test {
    ($tr:expr, $name:ident, $src:expr,$expected:expr) => {
        #[test]
        fn $name() {
            use ratel_visitor::Visitable;

            let module = ratel::parse($src).expect("failed to parse actual.js");
            let mut tr = {
                let ctx = ::TransformerCtxt {
                    arena: module.arena(),
                    scope: ::scope::analyze(&module),
                };
                $tr(ctx)
            };
            module.visit_with(&mut tr);

            let expected = ratel::parse($expected).expect("failed parse expected.js");

            assert_eq!(module.body(), expected.body());
        }
    };
}
