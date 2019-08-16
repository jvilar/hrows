module GUI.CanBeCast (
            -- *Classes
            CanBeCast(..)
) where

import GI.Gtk hiding (MessageDialog)

class GObject a => CanBeCast a where
    doCast :: GObject o => o -> IO a

instance CanBeCast Button where
    doCast = unsafeCastTo Button

instance CanBeCast CheckButton where
    doCast = unsafeCastTo CheckButton

instance CanBeCast ComboBox where
    doCast = unsafeCastTo ComboBox

instance CanBeCast ComboBoxText where
    doCast = unsafeCastTo ComboBoxText

instance CanBeCast Dialog where
    doCast = unsafeCastTo Dialog

instance CanBeCast Entry where
    doCast = unsafeCastTo Entry

instance CanBeCast FileChooserDialog where
    doCast = unsafeCastTo FileChooserDialog

instance CanBeCast Grid where
    doCast = unsafeCastTo Grid

instance CanBeCast Label where
    doCast = unsafeCastTo Label

instance CanBeCast Menu where
    doCast = unsafeCastTo Menu

instance CanBeCast MenuItem where
    doCast = unsafeCastTo MenuItem

instance CanBeCast Window where
    doCast = unsafeCastTo Window

