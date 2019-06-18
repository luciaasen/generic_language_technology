module IDE

import Prelude;
import util::IDE;
import util::ValueUI;

import vis::Figure;
import vis::Render;

import Syntax;

//  define the language name and extension

private str Pico_NAME = "Pico";
private str Pico_EXT = "pico";

//  Define the connection with the Pico parser
Tree parser(str x, loc l) {
    return parse(#Program, x, l);
}

public void registerPico() {
  registerLanguage(Pico_NAME, Pico_EXT, parser);
}