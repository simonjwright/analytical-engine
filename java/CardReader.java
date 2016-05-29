
import java.util.*;

//  A single Card

class Card {
    String text;                      // Contents of card
    int index;                        // Index in chain of cards
    CardSource source;                // Index in list of sources

    public Card(String s, int i, CardSource si) {
        text = s;
        index = i;
        source = si;
    }

    public String toString() {
        String s = "";

        s += (index + 1) + ". (" + source.sourceName + ":" +
                ((index - source.startIndex) + 1) + ") " + text;
        return s;
    }
}

//  Card Source Descriptors (non-period: for debugging)

class CardSource {
    String sourceName;                // Source of card (usually file name)
    int startIndex;                   // First index from this source

    public CardSource(String sn, int si) {
        sourceName = sn;
        startIndex = si;
    }
}

//  The Card Reader

class CardReader {
    private AnnunciatorPanel panel;
    private Attendant attendant;
    private Vector cards = null;      // No cards initially mounted
    private int ncards = 0;
    private int nextCardNumber = 0;   // Next card to read

    CardReader(AnnunciatorPanel p, Attendant a) {
        panel = p;
        attendant = a;
        reset();
    }

    public void reset() {
        panel.mountCardReaderChain(cards = null);
        ncards = 0;
        nextCardNumber = 0;
    }

    public void firstCard() {
        nextCardNumber = 0;
        if (ncards > 0) {
            panel.changeCardReaderCard((Card) cards.elementAt(nextCardNumber),
                                       nextCardNumber);
        }
    }

    public Card nextCard() {
        try {
            Card c = (Card) cards.elementAt(nextCardNumber);
            nextCardNumber++;
            panel.changeCardReaderCard(c, nextCardNumber);
            return c;
        } catch (ArrayIndexOutOfBoundsException e) {
            return null;
        }
    }

    public boolean advance(int n) {
        if ((nextCardNumber + n) >= ncards) {
            return false;
        }
        nextCardNumber += n;
        return true;
    }

    public boolean repeat(int n) {
        if ((nextCardNumber - n) < 0) {
            return false;
        }
        nextCardNumber -= n;
        return true;
    }

    public void listAll(OutputApparatus apparatus, boolean verbatim) {
        int i;

        for (i = 0; i < ncards; i++) {
            if (verbatim) {
                apparatus.Output(((Card) cards.elementAt(i)).text);
            } else {
                apparatus.Output(cards.elementAt(i).toString() + "\n");
            }
        }
    }

    //  Mount a chain of cards in the reader

    public void mountCards(Vector cardChain) {
        if (cardChain != null) {
            reset();
        }
        panel.mountCardReaderChain(cards = cardChain);
        ncards = cards.size();
    }
}
