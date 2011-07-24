package scala.collection.mutable;

public abstract class ConcurrentTrieBase<K, V> {
    
    public volatile Object root = null;
    
    //protected Object RESTART = INodeBase.RESTART;
    
}