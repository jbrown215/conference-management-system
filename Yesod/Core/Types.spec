module spec Yesod.Core.Types where

data HandlerT site m a <p :: User -> Bool> = Yesod.Core.Types.HandlerT {
        unHandlerT :: Yesod.Core.Types.HandlerData site (Yesod.Core.Types.MonadRoute m) -> m a
        }

data variance HandlerT covariant covariant covariant contravariant

measure content :: HandlerT site m a -> a

instance Monad HandlerT where 
 >>= :: forall <p :: User -> Bool, f:: a -> b -> Bool>. 
        x:HandlerT <p> site m a
     -> (u:a -> HandlerT <p> site m (b <f u>))
     -> HandlerT <p> site m (b<f (content x)>); 
 >>  :: x:HandlerT site m a
     -> HandlerT site m b
     -> HandlerT site m b;
 return :: forall <p :: User -> Bool>. a -> HandlerT <p> site m a 

