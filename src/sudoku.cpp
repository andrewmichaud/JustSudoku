#include <gtkmm.h>
#include <gtkmm/messagedialog.h>
int main(int argc, char *argv[]) 
{
    Glib::RefPtr<Gtk::Application> app = 
        Gtk::Application::create(argc, argv,
                "org.gtkmm.examples.base");

    Gtk::Window window;
    window.set_default_size(200,200);
    Gtk::Frame frame;
    window.add(frame);
    Gtk::Box box;
    frame.add(box);
    Gtk::Button button;
    Gtk::Button button2;
    box.pack_start(button);
    box.pack_start(button2);

    return app->run(window);
}
