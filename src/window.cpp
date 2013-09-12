#include "window.h"
#include <cstdio>
#include <iostream>
#include <string>

ExampleWindow::ExampleWindow()
: m_Button_Close("Close")
{
  set_title("Gtk::ScrolledWindow example");
  set_border_width(0);
  set_size_request(300, 300);

  m_ScrolledWindow.set_border_width(10);

  /* the policy is one of Gtk::POLICY AUTOMATIC, or Gtk::POLICY_ALWAYS.
   * Gtk::POLICY_AUTOMATIC will automatically decide whether you need
   * scrollbars, whereas Gtk::POLICY_ALWAYS will always leave the scrollbars
   * there.  The first one is the horizontal scrollbar, the second,
   * the vertical. */
  m_ScrolledWindow.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);

  get_content_area()->pack_start(m_ScrolledWindow);

  /* set the spacing to 10 on x and 10 on y */
  m_Grid.set_row_spacing(10);
  m_Grid.set_column_spacing(10);

  /* pack the grid into the scrolled window */
  m_ScrolledWindow.add(m_Grid);

  // Sudoku values.
  std::string initValues[] = {
      " ", "2", " ",  "3", " ", " ",  " ", "4", " ",
      " ", "1", " ",  " ", "2", " ",  " ", "3", " ",
      " ", " ", " ",  " ", " ", " ",  " ", " ", " ",

      " ", " ", " ",  " ", " ", " ",  " ", " ", " ",
      " ", "4", " ",  " ", "5", " ",  " ", "6", " ",
      " ", " ", " ",  " ", " ", " ",  " ", " ", " ",

      " ", " ", " ",  " ", " ", " ",  " ", " ", " ",
      " ", "7", " ",  " ", "8", " ",  " ", "9", " ",
      " ", " ", " ",  " ", " ", " ",  " ", " ", " "};
                                                            
  
  /* this simply creates a grid of toggle buttons
   * to demonstrate the scrolled window. */
  Glib::ustring curString;
  int row = 0;
  int col = 0;
  guint oldRowSpacing;
  guint oldColSpacing;

  for(int i = 0; i != 81; ++i) {
        curString = Glib::ustring(initValues[i]);
        Gtk::Button* pButton = Gtk::manage(new Gtk::ToggleButton());
        pButton->set_label(curString);
        pButton->set_size_request (30, 30);
 
        m_Grid.attach(*pButton, col, row, 1, 1);
        
        if ((i + 1) % 27 == 0) {
            std::cout << "mod 27" << std::endl;
            row += 1;
            col = 0;
            m_Grid.set_row_spacing(10);

        } else if ((i + 1) % 9 == 0) {
            std::cout << "mod 9" << std::endl;
            row += 1;
            col = 0;
            m_Grid.set_row_spacing(0);

        } else if ((i + 1) % 3 == 0) {
            std::cout << "mod 3" << std::endl;
            col += 1;
            m_Grid.set_column_spacing(10);

        } else {
            std::cout << "mod 1" << std::endl;
            col += 1;
            m_Grid.set_column_spacing(0);

        }
  }

  /* Add a "close" button to the bottom of the dialog */
  m_Button_Close.signal_clicked().connect( sigc::mem_fun(*this,
              &ExampleWindow::on_button_close));

  /* this makes it so the button is the default. */
  m_Button_Close.set_can_default();

  Gtk::Box* pBox = get_action_area();
  if(pBox)
    pBox->pack_start(m_Button_Close);

  /* This grabs this button to be the default button. Simply hitting
   * the "Enter" key will cause this button to activate. */
  m_Button_Close.grab_default();

  show_all_children();
}

ExampleWindow::~ExampleWindow()
{
}

void ExampleWindow::on_button_close()
{
  hide();
}
